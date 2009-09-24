;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Gambit: Compile Script

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

;;; Note: If you are using Gambit prior to v4.5.3, an internal compiler
;;; bug will cause this to fail; you will need to load this code into
;;; the interpreter.

(if (not (eq? 'foo 'FOO))
    (error "Please run Gambit in standards-compliant mode with `-:s'."))

;;; Bizarrely, although it operates at macro-expand-time, INCLUDE makes
;;; the macros defined in these files visible to COMPILE-FILE.  Perhaps
;;; this is only the case in the interpreter, however.  I have not
;;; tried compiling this file.

(include "gambit-syntax.scm")
(include "rendezvous-syntax.scm")

;;; Apparently Gambit's compiler doesn't know how to deal with macros,
;;; so the macro-related files are not compiled here.

;; (compile-file "gambit-syntax")
(compile-file "gambit-etc")
(compile-file "sort")
(compile-file "queue")
(compile-file "srfi-18-suspend")
(compile-file "primitive")
(compile-file "rendezvous")
(compile-file "srfi-18-time")
;; (compile-file "rendezvous-syntax")
(compile-file "channel")
(compile-file "mailbox")
(compile-file "placeholder")
(compile-file "semaphore")
