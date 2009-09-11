;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Critical Sections: Thread Suspension

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

(define-record-type <suspension>
    (make-suspension token composition)
    suspension?
  (token suspension.token)
  (composition suspension.composition))

(define-record-type <suspension-token>
    (make-suspension-token cell-then-thunk uid)
    suspension-token?
  (cell-then-thunk suspension-token.cell-then-thunk
                   set-suspension-token.cell-then-thunk!)
  (uid suspension-token.uid))

(define *suspension-uid* 0)

(define (new-suspension-uid)
  (with-interrupts-inhibited
    (lambda ()
      (let ((uid *suspension-uid*))
        (set! *suspension-uid* (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (with-interrupts-inhibited
    (lambda ()
      (set! *suspension-uid* 0))))

(define (enter-critical-section procedure)
  (procedure (set-enabled-interrupts! no-interrupts)))

(define (exit-critical-section interrupts continuation)
  (set-enabled-interrupts! interrupts)
  (continuation))

(define (suspend interrupts procedure)
  (let* ((cell (make-cell (current-thread)))
         (token (make-suspension-token cell (new-suspension-uid))))
    (procedure
     (lambda (composition)
       (make-suspension token composition))
     (lambda ()
       (set-enabled-interrupts! interrupts)
       ((with-new-proposal (retry)
          (if (maybe-commit-and-block cell)
              (suspension-token.cell-then-thunk token)
              (retry)))))
     (lambda (continuation)
       (set-enabled-interrupts! interrupts)
       (continuation)))))

(define (maybe-resume suspension thunk)
  (let* ((token (suspension.token suspension))
         (interrupts (set-enabled-interrupts! no-interrupts)))
    (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
      (if (cell? cell-then-thunk)
          (begin
            (set-suspension-token.cell-then-thunk!
             token
             (let ((composition (suspension.composition suspension)))
               (lambda ()
                 (composition thunk))))
            (set-enabled-interrupts! interrupts)
            (with-new-proposal (retry)
              (if (maybe-commit-and-make-ready (cell-ref cell-then-thunk))
                  #t
                  (retry))))
          (begin
            (set-enabled-interrupts! interrupts)
            #f)))))
