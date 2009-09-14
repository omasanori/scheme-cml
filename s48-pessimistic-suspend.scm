;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Pessimistic: Thread Suspension

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
    (%make-suspension-token lock cell-then-thunk uid)
    suspension-token?
  (lock suspension-token.lock)
  (cell-then-thunk suspension-token.cell-then-thunk
                   set-suspension-token.cell-then-thunk!)
  (uid suspension-token.uid))

(define (make-suspension-token)
  (%make-suspension-token (make-lock)
                          (make-cell (current-thread))
                          (new-suspension-uid)))

(define *suspension-uid* 0)
(define suspension-uid-lock (make-lock))

(define (new-suspension-uid)
  (with-lock suspension-uid-lock
    (lambda ()
      (let ((uid *suspension-uid*))
        (set! *suspension-uid* (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (with-lock suspension-uid-lock
    (lambda ()
      (set! *suspension-uid* 0))))

(define (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define (suspend critical-token procedure)
  critical-token                        ;ignore
  (let ((token (make-suspension-token)))
    ((with-lock (suspension-token.lock token)
       (lambda ()
         (procedure
          (lambda (composition)
            (make-suspension token composition))
          (lambda ()
            (let ((cell (suspension-token.cell-then-thunk token)))
              (let loop ()
                ;; Disabling interrupts makes lock-and-block atomic.
                (let ((interrupts (set-enabled-interrupts! no-interrupts)))
                  (release-lock (suspension-token.lock token))
                  (block cell)
                  (set-enabled-interrupts! interrupts)
                  (obtain-lock (suspension-token.lock token))
                  (let ((cell-then-thunk
                         (suspension-token.cell-then-thunk token)))
                    (if (cell? cell-then-thunk)
                        (loop)
                        cell-then-thunk))))))
          (lambda (continuation)
            continuation)))))))

(define (maybe-resume suspension thunk)
  (let ((token (suspension.token suspension)))
    (with-lock (suspension-token.lock token)
      (lambda ()
        (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
          (if (cell? cell-then-thunk)
              (begin
                (set-suspension-token.cell-then-thunk!
                 token
                 (let ((composition (suspension.composition suspension)))
                   (lambda ()
                     (composition thunk))))
                (make-ready cell-then-thunk)
                #t)
              #f))))))

(define (with-suspension-claimed suspension if-claimed if-not-claimed)
  (let ((token (suspension.token suspension)))
    (obtain-lock (suspension-token.lock token))
    (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
      (if (cell? cell-then-thunk)
          (if-claimed
           (let ((composition (suspension.composition suspension)))
             (lambda (thunk)
               (set-suspension-token.cell-then-thunk!
                token
                (lambda ()
                  (composition thunk)))
               (make-ready cell-then-thunk)
               (release-lock (suspension-token.lock token))))
           (lambda ()
             (release-lock (suspension-token.lock token))))
          (begin
            (release-lock (suspension-token.lock token))
            (if-not-claimed))))))

(define (block cell)
  (with-new-proposal (retry)
    (if (not (maybe-commit-and-block cell))
        (retry))))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))

;;; For our purposes in this file, this single-value version suffices.

(define (with-lock lock body)
  (obtain-lock lock)
  (let ((result (body)))
    (release-lock lock)
    result))
