;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Interrupt-Based: Thread Suspension

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
    (%make-suspender cell owner state)
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
    (let spin ()
      (let ((interrupts (set-enabled-interrupts! no-interrupts)))
        (if (suspender/locked? suspender)
            (begin
              (set-enabled-interrupts! interrupts)
              (relinquish-timeslice)
              (spin))
            (begin
              (set-suspender.owner! suspender me)
              (set-enabled-interrupts! interrupts)))))))

(define (suspender/unlock suspender)
  (assert-suspender-lock suspender)
  ;; Disabling interrupts here is probably superfluous.
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    (set-suspender.owner! suspender #f)
    (set-enabled-interrupts! interrupts)))

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
  ;; Interrupts are disabled on entry to LOOP so that the action of
  ;; unlocking the suspender and blocking the thread is atomic.
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    (let loop ()
      (set-suspender.owner! suspender #f)
      ;; BLOCK enables interrupts when it returns.
      (block (suspender.cell suspender))
      (let spin ()
        (let ((interrupts* (set-enabled-interrupts! no-interrupts)))
          (if (suspender/locked? suspender)
              (begin
                (set-enabled-interrupts! interrupts*)
                (relinquish-timeslice)
                (spin))
              (set-suspender.owner! suspender (current-thread)))))
      ;; Suspender is locked and interrupts are disabled.
      (let ((state (suspender.state suspender)))
        (cond ((pair? state)
               (set-suspender.state! suspender #f)
               (set-enabled-interrupts! interrupts)
               (car state))
              ((eq? state (current-thread)) (loop)) ;Sanity check.
              (else (error "Suspender reused or aborted:" suspender)))))))

(define (block cell)
  (with-new-proposal (retry)
    ;; This proposal should not fail, because it doesn't do anything.
    ;; If it does fail, we just pretend that some thread woke us, and
    ;; the caller is expected to check whatever condition we were
    ;; waiting for, and retry blocking if the condition was not set.
    ;; The reason we can't use RETRY is that MAYBE-COMMIT-AND-BLOCK
    ;; re-enables interrupts if it fails, so that another thread might
    ;; swoop in, set the condition, and try to make this thread ready
    ;; before this thread has even blocked.  Then this thread would
    ;; block with nobody to make it ready.
    retry                               ;ignore
    (maybe-commit-and-block cell))
  (values))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))
