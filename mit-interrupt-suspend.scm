;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; MIT Scheme, Interrupt-Based: Thread Suspension

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

(declare (usual-integrations))

(define-integrable (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define-integrable (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define-record-type <suspender>
    (%make-suspender thread locked? set? value)
    suspender?
  (thread suspender.thread)
  (locked? suspender.locked? set-suspender.locked?!)
  (set? suspender.set? set-suspender.set?!)
  (value suspender.value set-suspender.value!))

(define (make-suspender)
  (%make-suspender (current-thread) #f #f #f))

(define (suspender/lock suspender)
  (let spin ()
    (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
      (if (suspender.locked? suspender)
          (begin
            (set-interrupt-enables! interrupt-mask)
            (yield-current-thread)
            (spin))
          (begin
            (set-suspender.locked?! suspender #t)
            (set-interrupt-enables! interrupt-mask))))))

(define (suspender/unlock suspender)
  ;; Disabling interrupts here is probably superfluous.
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (set-suspender.locked?! suspender #f)
    (set-interrupt-enables! interrupt-mask)))

(define (suspender/resumed? suspender)
  (suspender.set? suspender))

(define (suspender/resume suspender value)
  (set-suspender.set?! suspender #t)
  (set-suspender.value! suspender value)
  (signal-thread-event (suspender.thread suspender) #t))

(define (suspender/abort suspender)
  (set-suspender.set?! suspender #t))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  ;; Interrupts must be enabled on entry to LOOP, so that unlocking the
  ;; suspender and suspending happen atomically.  We could instead
  ;; block thread events, but we also need interrupts disabled in order
  ;; to lock the suspender again.  Since SUSPEND-CURRENT-THREAD and
  ;; YIELD-CURRENT-THREAD let other threads run but save and restore
  ;; the interrupt mask to whatever it was on entry, it is convenient
  ;; just to save the interrupt mask at the beginning and restore it at
  ;; the very end, and not fiddle with it in the middle.
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let loop ()
      (set-suspender.locked?! suspender #f)
      (suspend-current-thread)
      (let spin ()
        (if (suspender.locked? suspender)
            (begin (yield-current-thread)
                   (spin))
            (set-suspender.locked?! suspender #t)))
      (if (suspender.set? suspender)
          (let ((value (suspender.value suspender)))
            (set-suspender.value! suspender #f)
            (set-interrupt-enables! interrupt-mask)
            value)
          (loop)))))
