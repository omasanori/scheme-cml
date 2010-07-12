;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Optimistic: Thread Suspension

;;; Copyright (c) 2009, 2010, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define-synchronized-record-type suspender <suspsender>
    (%make-suspender cell owner state)
    (owner state)
    suspender?
  (cell suspender.cell)
  (owner suspender.owner set-suspender.owner!)
  ;; Possible values of the state field:
  ;;   #f -> aborted or already used
  ;;   <thread> -> waiting, if alive
  ;;   (<value>) -> resumed with <value>
  (state suspender.state set-suspender.state!))

(define (make-suspender)
  (%make-suspender (make-cell (current-thread)) #f (current-thread)))

(define (assert-suspender-lock suspender)
  (let ((me (current-thread)))
    (if (not (eq? me (suspender.owner suspender)))
        (error "I don't have this suspender locked:" suspender me))))

(define (live-thread? object)
  (and (thread? object)
       (or (thread-continuation object)
           (running? object))
       #t))

(define (suspender/locked? suspender)
  (live-thread? (suspender.owner suspender)))

;;; Important: SUSPENDER/LOCK locks the suspender even if it was locked
;;; by a thread that has terminated.  Thus, we must keep the suspender
;;; in a consistent state even while it is locked.

(define (suspender/lock suspender)
  (let ((me (current-thread)))
    (if (eq? me (suspender.owner suspender))
        (error "I already have this suspender locked:" suspender me))
    (call-ensuring-atomicity!
     (lambda ()
       (if (suspender/locked? suspender)
           ;; Spin the transaction.
           (begin
             (invalidate-current-proposal!)
             (relinquish-timeslice)))
       ;; For the current transaction, give the simulacrum of having
       ;; acquired it, as SUSPENDER/LOCK is supposed to guarantee.  Of
       ;; course, this effect will never be made global if someone else
       ;; already held the lock, as INVALIDATE-CURRENT-PROPOSAL! will
       ;; guarantee.
       (set-suspender.owner! suspender me)))))

(define (suspender/unlock suspender)
  (assert-suspender-lock suspender)
  ;; Ensuring atomicity here is probably superfluous.
  (call-ensuring-atomicity!
    (lambda ()
      (set-suspender.owner! suspender #f))))

(define (suspender/resumed? suspender)
  (assert-suspender-lock suspender)
  (not (live-thread? (suspender.state suspender))))

(define (suspender/resume suspender value)
  (assert-suspender-lock suspender)
  (error-if-current-proposal)
  ;; The order of actions here is important.  It is safe to make the
  ;; suspended thread ready before we have updated the state -- that
  ;; thread won't try to read the state until we unlock the suspender.
  ;; But if we updated the state first and were interrupted before we
  ;; got a chance to make the suspended thread ready, then the
  ;; suspender would be set, and the suspended thread would never know.
  (with-new-proposal (retry)
    (if (not
         (maybe-commit-and-make-ready (cell-ref (suspender.cell suspender))))
        (retry)))
  (set-suspender.state! suspender (list value)))

(define (suspender/abort suspender)
  (assert-suspender-lock suspender)
  (set-suspender.state! suspender #f))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  (assert-suspender-lock suspender)
  (error-if-current-proposal)
  (let loop ()
    (with-new-proposal (retry)
      (set-suspender.owner! suspender #f)
      (if (not (maybe-commit-and-block (suspender.cell suspender)))
          (retry)))
    ((with-new-proposal (retry)
       (if (suspender/locked? suspender)
           (begin
             ;; Spin the transaction.  Invalidating the proposal is not
             ;; necessary -- we can just discard it altogether.  On a
             ;; multiprocessor system, we ought to relinquish our time
             ;; slice only if the owner is on the same processor.
             (relinquish-timeslice)
             (retry))
           (begin
             (set-suspender.owner! suspender (current-thread))
             (let ((state (suspender.state suspender)))
               (cond ((pair? state)
                      (set-suspender.state! suspender #f)
                      (if (maybe-commit) (lambda () (car state)) (retry)))
                     ((eq? state (current-thread)) ;Sanity check.
                      (if (maybe-commit) loop (retry)))
                     (else
                      (if (maybe-commit)
                          (lambda ()
                            (error "Suspender reused or aborted:" suspender))
                          (retry)))))))))))

(define (error-if-current-proposal)
  (if (current-proposal)
      (error "I am about to perform an action that cannot be rolled back!")))
