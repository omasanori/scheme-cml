;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Gambit: Locked Record Type Definition Syntax, &c.
;;;; ...with SYNTAX-CASE to work around Gambit's broken hygiene.

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

(define-syntax define-locked-record-type
  (lambda (form)
    (syntax-case form ()
      ((DEFINE-LOCKED-RECORD-TYPE name
           (constructor constructor-field ...)
           (locked-field ...)
           predicate
           with-locked-object
         (field field-accessors ...)
         ...)
       (let ((context (syntax DEFINE-LOCKED-RECORD-TYPE)))
         (with-syntax ((%constructor
                        (datum->syntax-object
                         context
                         (gensym (syntax-object->datum (syntax constructor)))))
                       (mutex-field
                        (datum->syntax-object context (gensym 'MUTEX)))
                       (object.mutex
                        (datum->syntax-object context (gensym 'OBJECT.MUTEX))))
           (syntax
            (BEGIN
              (DEFINE-RECORD-TYPE name
                  (%constructor mutex-field constructor-field ...)
                  predicate
                (mutex-field object.mutex)
                (field field-accessors ...)
                ...)
              (DEFINE (constructor constructor-field ...)
                (%constructor (MAKE-MUTEX) constructor-field ...))
              (DEFINE (with-locked-object OBJECT PROCEDURE)
                (MUTEX-LOCK! (object.mutex OBJECT))
                (CALL-WITH-VALUES PROCEDURE
                  (LAMBDA RESULTS
                    (MUTEX-UNLOCK! (object.mutex OBJECT))
                    (APPLY VALUES RESULTS))))))))))))

(define-syntax receive
  (syntax-rules ()
    ((RECEIVE bound-variable-list expression body0 body1+ ...)
     (CALL-WITH-VALUES (LAMBDA () expression)
       (LAMBDA bound-variable-list body0 body1+ ...)))))
