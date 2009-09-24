;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Gambit: Miscellanea

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

(define (spawn thunk . name)
  (thread-start! (apply make-thread thunk name)))

(define (sleep milliseconds)
  (thread-sleep! (/ milliseconds 1000)))

(define (compose-nullary procedure nullary-procedure)
  (lambda ()
    (call-with-values nullary-procedure procedure)))

(define (compose-unary procedure unary-procedure)
  (lambda (argument)
    (call-with-values (lambda ()
                        (unary-procedure argument))
      procedure)))

(define (reduce-right procedure identity list)
  (if (null-list? list)
      identity
      (fold-right procedure (car list) (cdr list))))

(define (fold-right procedure initial-value list)
  (if (null-list? list)
      initial-value
      (procedure (car list)
                 (fold-right procedure initial-value (cdr list)))))

(define (null-list? object)
  (if (pair? object)
      #f
      (begin
        (if (not (null? object))
            (error "Non-list:" object))
        #t)))
