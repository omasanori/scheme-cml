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

(define-record-type <suspension>
    (make-suspension token composition)
    suspension?
  (token suspension.token)
  (composition suspension.composition))

(define-synchronized-record-type suspension-token <suspension-token>
    (make-suspension-token cell-then-thunk uid)
    (cell-then-thunk)
    suspension-token?
  (cell-then-thunk suspension-token.cell-then-thunk
                   set-suspension-token.cell-then-thunk!)
  (uid suspension-token.uid))

(define suspension-uid-cell (make-cell 0))

(define (new-suspension-uid)
  (call-ensuring-atomicity
    (lambda ()
      (let ((uid (provisional-cell-ref suspension-uid-cell)))
        (provisional-cell-set! suspension-uid-cell (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (call-ensuring-atomicity
    (lambda ()
      (provisional-cell-set! suspension-uid-cell 0))))

(define (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define (suspend critical-token procedure)
  critical-token                        ;ignore
  (let* ((cell (make-cell (current-thread)))
         (token (make-suspension-token cell (new-suspension-uid))))
    (procedure
     (lambda (composition)
       (make-suspension token composition))
     (lambda ()
       ((with-new-proposal (retry)
          (if (maybe-commit-and-block cell)
              (suspension-token.cell-then-thunk token)
              (retry)))))
     (lambda (continuation)
       (continuation)))))

(define (maybe-resume suspension thunk)
  (let ((token (suspension.token suspension)))
    (with-new-proposal (retry)
      (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
        (if (cell? cell-then-thunk)
            (begin
              (set-suspension-token.cell-then-thunk!
               token
               (let ((composition (suspension.composition suspension)))
                 (lambda ()
                   (composition thunk))))
              (if (maybe-commit-and-make-ready (cell-ref cell-then-thunk))
                  #t
                  (retry)))
            #f)))))
