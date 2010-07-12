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
    (%make-suspender cell locked? set? value)
    suspender?
  (cell suspender.cell)
  (locked? suspender.locked? set-suspender.locked?!)
  (set? suspender.set? set-suspender.set?!)
  (value suspender.value set-suspender.value!))

(define (make-suspender)
  (%make-suspender (make-cell (current-thread)) #f #f #f))

(define (suspender/lock suspender)
  (let spin ()
    (let ((interrupts (set-enabled-interrupts! no-interrupts)))
      (if (suspender.locked? suspender)
          (begin
            (set-enabled-interrupts! interrupts)
            (relinquish-timeslice)
            (spin))
          (begin
            (set-suspender.locked?! suspender #t)
            (set-enabled-interrupts! interrupts))))))

(define (suspender/unlock suspender)
  ;; Disabling interrupts here is probably superfluous.
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    (set-suspender.locked?! suspender #f)
    (set-enabled-interrupts! interrupts)))

(define (suspender/resumed? suspender)
  (suspender.set? suspender))

(define (suspender/resume suspender value)
  (set-suspender.set?! suspender #t)
  (set-suspender.value! suspender value)
  (make-ready (suspender.cell suspender)))

(define (suspender/abort suspender)
  (set-suspender.set?! suspender #t))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  ;; Interrupts are disabled on entry to LOOP so that the action of
  ;; unlocking the suspender and blocking the thread is atomic.
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    (let loop ()
      (set-suspender.locked?! suspender #f)
      ;; BLOCK enables interrupts when it returns.
      (block (suspender.cell suspender))
      (let spin ()
        (let ((interrupts* (set-enabled-interrupts! no-interrupts)))
          (if (suspender.locked? suspender)
              (begin
                (set-enabled-interrupts! interrupts*)
                (relinquish-timeslice)
                (spin))
              (set-suspender.locked?! suspender #t))))
      ;; Suspender is locked and interrupts are disabled.
      (if (suspender.set? suspender)
          (let ((value (suspender.value suspender)))
            (set-suspender.value! suspender #f)
            (set-enabled-interrupts! interrupts)
            value)
          (loop)))))

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
