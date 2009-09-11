;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; MIT Scheme, Interrupt-Based: Thread Suspension

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

(declare (usual-integrations))

(define-record-type <suspension>
    (make-suspension token composition)
    suspension?
  (token suspension.token)
  (composition suspension.composition))

(define-record-type <suspension-token>
    (make-suspension-token thread-then-thunk uid)
    suspension-token?
  (thread-then-thunk suspension-token.thread-then-thunk
                     set-suspension-token.thread-then-thunk!)
  (uid suspension-token.uid))

(define *suspension-uid* 0)

(define (new-suspension-uid)
  (without-interrupts
    (lambda ()
      (let ((uid *suspension-uid*))
        (set! *suspension-uid* (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (without-interrupts
    (lambda ()
      (set! *suspension-uid* 0))))

(define-integrable (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define-integrable (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define (suspend critical-token procedure)
  critical-token                        ;ignore
  (let ((token (make-suspension-token (current-thread) (new-suspension-uid))))
    (procedure
     (lambda (composition)
       (make-suspension token composition))
     (lambda ()
       (let loop ()
         (suspend-current-thread)
         ((without-interrupts
            (lambda ()
              (let ((thread-then-thunk
                     (suspension-token.thread-then-thunk token)))
                (if (thread? thread-then-thunk)
                    loop
                    thread-then-thunk)))))))
     (lambda (continuation)
       (continuation)))))

(define (maybe-resume suspension thunk)
  (let* ((token (suspension.token suspension))
         (interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((thread-then-thunk (suspension-token.thread-then-thunk token)))
      (if (thread? thread-then-thunk)
          (begin
            (set-suspension-token.thread-then-thunk!
             token
             (let ((composition (suspension.composition suspension)))
               (lambda ()
                 (composition thunk))))
            (set-interrupt-enables! interrupt-mask)
            (signal-thread-event thread-then-thunk #t)
            #t)
          (begin
            (set-interrupt-enables! interrupt-mask)
            #f)))))
