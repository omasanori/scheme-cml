;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Optimistic: Thread Suspension

;;; Copyright (c) 2009, Taylor R. Campbell
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
    (%make-suspender cell locked? set? value)
    (locked? set? value)
    suspender?
  (cell suspender.cell)
  (locked? suspender.locked? set-suspender.locked?!)
  (set? suspender.set? set-suspender.set?!)
  (value suspender.value set-suspender.value!))

(define (make-suspender)
  (%make-suspender (make-cell (current-thread)) #f #f #f))

(define (suspender/lock suspender)
  (call-ensuring-atomicity!
    (lambda ()
      (if (suspender.locked? suspender)
          ;; Spin the transaction.
          (begin
            (invalidate-current-proposal!)
            (relinquish-timeslice))
          (set-suspender.locked?! suspender #t)))))

(define (suspender/unlock suspender)
  ;; Ensuring atomicity here is probably superfluous.
  (call-ensuring-atomicity!
    (lambda ()
      (set-suspender.locked?! suspender #f))))

(define (suspender/resumed? suspender)
  (suspender.set? suspender))

(define (suspender/resume suspender value)
  (error-if-current-proposal)
  (set-suspender.set?! suspender #t)
  (set-suspender.value! suspender value)
  (with-new-proposal (retry)
    (if (not
         (maybe-commit-and-make-ready (cell-ref (suspender.cell suspender))))
        (retry))))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  (error-if-current-proposal)
  (let loop ()
    (with-new-proposal (retry)
      (set-suspender.locked?! suspender #f)
      (if (not (maybe-commit-and-block (suspender.cell suspender)))
          (retry)))
    ((with-new-proposal (retry)
       (cond ((suspender.locked? suspender)
              ;; Spin the transaction.  Invalidating the proposal is
              ;; not necessary -- we can just discard it altogether.
              ;; On a multiprocessor system, we ought instead to record
              ;; who owns the lock, and to relinquish our time slice
              ;; only if the owner is on the same processor.
              (relinquish-timeslice)
              (retry))
             ((suspender.set? suspender)
              (let ((value (suspender.value suspender)))
                (set-suspender.value! suspender #f)
                (if (maybe-commit)
                    (lambda () value)
                    (retry))))
             (else
              (set-suspender.locked?! suspender #t)
              (if (maybe-commit)
                  loop
                  (retry))))))))

(define (error-if-current-proposal)
  (if (current-proposal)
      (error "I am about to perform an action that cannot be rolled back!")))
