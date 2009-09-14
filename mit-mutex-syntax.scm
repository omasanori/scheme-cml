;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; MIT Scheme, Mutex-Based: Syntax

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

(define-syntax define-locked-record-type
  (er-macro-transformer
   (lambda (form rename compare)
     rename compare                     ;ignore
     `(,(rename '%DEFINE-LOCKED-RECORD-TYPE)
       ,(generate-uninterned-symbol (symbol-name 'CONSTRUCTOR))
       ,(generate-uninterned-symbol (symbol-name 'MUTEX))
       ,(generate-uninterned-symbol (symbol-name 'MUTEX-ACCESSOR))
       ,@(cdr form)))))

(define-syntax %define-locked-record-type
  (syntax-rules ()
    ((%DEFINE-LOCKED-RECORD-TYPE %constructor mutex-field mutex-accessor name
         (constructor constructor-field ...)
         (locked-field ...)
         predicate
         with-locked-object
       (field field-accessors ...)
       ...)
     (BEGIN
       (DEFINE-RECORD-TYPE name
           (%constructor mutex-field constructor-field ...)
           predicate
         (mutex-field mutex-accessor)
         (field field-accessors ...)
         ...)
       (DEFINE (constructor constructor-field ...)
         (%constructor (MAKE-THREAD-MUTEX) constructor-field ...))
       (DEFINE (with-locked-object OBJECT PROCEDURE)
         (WITH-THREAD-MUTEX-LOCKED (mutex-accessor OBJECT)
           PROCEDURE))))))