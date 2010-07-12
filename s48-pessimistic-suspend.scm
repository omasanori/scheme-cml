;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Pessimistic: Thread Suspension

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

(define-record-type <suspender>
    (%make-suspender lock cell state)
    suspender?
  (lock suspender.lock)
  (cell suspender.cell)
  ;; Possible values of the state field:
  ;;   #f -> aborted or already used
  ;;   <thread> -> waiting, if alive
  ;;   (<value>) -> resumed with <value>
  (state suspender.state set-suspender.state!))

(define (make-suspender)
  (%make-suspender (make-lock) (make-cell (current-thread)) (current-thread)))

(define (assert-suspender-lock suspender)
  ;; No way to get at the lock.
  (values))

(define (live-thread? object)
  (and (thread? object)
       (or (thread-continuation object)
           (running? object))
       #t))

;;; SUSPENDER/LOCK is supposed to lock the suspender even if it was
;;; locked by a thread that has terminated.  However, we can't do that
;;; with Scheme48's locks.  See the other suspender implementations for
;;; comparison.

(define (suspender/lock suspender)
  ;; Unfortunately, this will block forever if a thread terminated
  ;; while holding the lock, or if the current thread already has the
  ;; lock, and there is no way to detect either situation.
  (obtain-lock (suspender.lock suspender)))

(define (suspender/unlock suspender)
  (assert-suspender-lock suspender)
  (release-lock (suspender.lock suspender)))

(define (suspender/resumed? suspender)
  (assert-suspender-lock suspender)
  (not (live-thread? (suspender.state suspender))))

(define (suspender/resume suspender value)
  (assert-suspender-lock suspender)
  ;; The order of actions here is important.  It is safe to make the
  ;; suspended thread ready before we have updated the state -- that
  ;; thread won't try to read the state until we unlock the suspender.
  ;; But if we updated the state first and were interrupted before we
  ;; got a chance to make the suspended thread ready, then the
  ;; suspender would be set, and the suspended thread would never know.
  (make-ready (suspender.cell suspender))
  (set-suspender.state! suspender (list value)))

(define (suspender/abort suspender)
  (assert-suspender-lock suspender)
  (set-suspender.state! suspender #f))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  (assert-suspender-lock suspender)
  (let loop ()
    (release-lock-and-block (suspender.lock suspender)
                            (suspender.cell suspender))
    (obtain-lock (suspender.lock suspender))
    (let ((state (suspender.state suspender)))
      (cond ((pair? state) (set-suspender.state! suspender #f) (car state))
            ((eq? state (current-thread)) (loop))
            (else (error "Suspender reused or aborted:" suspender))))))

(define (release-lock-and-block lock cell)
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    ;++ Bug: RELEASE-LOCK is not atomic even with interrupts disabled:
    ;++ it may let other threads run in the process of readying them.
    (release-lock lock)
    (block cell)
    (set-enabled-interrupts! interrupts)))

(define (block cell)
  (with-new-proposal (retry)
    ;; See s48-interrupt-suspend.scm for why we ignore RETRY here.
    retry                               ;ignore
    (maybe-commit-and-block cell))
  (values))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))
