;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Interrupt-Based: Thread Suspension

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

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  ;; Interrupts are disabled on entry to LOOP so that the action of
  ;; unlocking the suspender and blocking the thread is atomic.
  (set-enabled-interrupts! no-interrupts)
  (let loop ()
    (set-suspender.locked?! suspender #f)
    ;; BLOCK enables interrupts when it returns.
    (block (suspender.cell suspender))
    (let spin ()
      (let ((interrupts (set-enabled-interrupts! no-interrupts)))
        (cond ((suspender.locked? suspender)
               (set-enabled-interrupts! interrupts)
               (relinquish-timeslice)
               (spin))
              ((suspender.set? suspender)
               (let ((value (suspender.value suspender)))
                 (set-suspender.value! suspender #f)
                 (set-enabled-interrupts! interrupts)
                 value))
              (else
               (set-suspender.locked?! suspender #t)
               ;; Leave interrupts disabled when entering LOOP.  It
               ;; doesn't matter that we forget what they were -- BLOCK
               ;; knows what to do.
               (loop)))))))

(define (block cell)
  (with-new-proposal (retry)
    (if (not (maybe-commit-and-block cell))
        (retry))))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))
