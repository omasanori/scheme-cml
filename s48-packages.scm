;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48: Package Definitions

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

(define-structures ((rendezvous rendezvous-interface)
                    (rendezvous-builder rendezvous-builder-interface))
  (open scheme
        critical-sections
        locked-record-types
        primitive-rendezvous
        receiving
        resumption
        srfi-1                          ;list-lib
        srfi-9                          ;define-record-types
        srfi-23                         ;error
        )
  (optimize auto-integrate)
  (begin
    (define (compose-nullary procedure nullary-procedure)
      (lambda ()
        (call-with-values nullary-procedure procedure))))
  (files rendezvous)
  (files rendezvous-syntax)
  (open time (subset threads-internal (register-dozer!)))
  (files s48-time))

(define-structures ((primitive-rendezvous primitive-rendezvous-interface)
                    (resumption resumption-interface))
  (open scheme
        critical-sections
        sorting
        srfi-1                          ;list-lib
        srfi-9                          ;define-record-type
        suspenders
        )
  (optimize auto-integrate)
  (begin
    (define (compose-unary procedure unary-procedure)
      (lambda (argument)
        (procedure (unary-procedure argument)))))
  (files primitive))

(define-structure rendezvous-channels rendezvous-channels-interface
  (open scheme
        locked-record-types
        optimistic-queues
        rendezvous
        rendezvous-builder
        )
  (optimize auto-integrate)
  (files channel))

(define-structure rendezvous-mailboxes rendezvous-mailboxes-interface
  (open scheme
        locked-record-types
        optimistic-queues
        receiving
        rendezvous
        rendezvous-builder
        )
  (optimize auto-integrate)
  (files mailbox))

(define-structure rendezvous-placeholders rendezvous-placeholders-interface
  (open scheme
        locked-record-types
        rendezvous
        rendezvous-builder
        srfi-23                         ;error
        )
  (optimize auto-integrate)
  (files placeholder))

(define-structure rendezvous-semaphores rendezvous-semaphores-interface
  (open scheme
        optimistic-queues
        rendezvous
        rendezvous-channels
        rendezvous-mailboxes
        rendezvous-placeholders
        srfi-9                          ;define-record-type
        threads
        )
  (optimize auto-integrate)
  (files semaphore))

;;; Correct bugs in Scheme48's queues.  (Unfortunately, they're what
;;; Scheme48's run-time system uses -- eeeek!)

(define-structure optimistic-queues (interface-of queues)
  (open scheme proposals srfi-23)
  (optimize auto-integrate)
  (files s48-optimistic-queue))

;;;; Parameters

(define-structures ((critical-sections/optimistic critical-sections-interface)
                    (suspenders/optimistic suspenders-interface))
  (open scheme
        cells
        ;; CURRENT-PROPOSAL is used only for sanity checks.
        (subset low-proposals (current-proposal))
        proposals
        srfi-23                         ;error
        threads
        threads-internal
        )
  (optimize auto-integrate)
  (files s48-optimistic-suspend))

(define-structure locked-record-types/optimistic locked-record-types-interface
  (open scheme proposals)
  (optimize auto-integrate)
  (files s48-optimistic-record))

(define-structures ((critical-sections/pessimistic critical-sections-interface)
                    (suspenders/pessimistic suspenders-interface))
  (open scheme
        cells
        interrupts
        locks
        proposals
        srfi-9                          ;define-record-type
        threads
        threads-internal
        )
  (optimize auto-integrate)
  (files s48-pessimistic-suspend))

(define-structure locked-record-types/pessimistic locked-record-types-interface
  (open scheme
        locks
        receiving
        srfi-9                          ;define-record-type
        )
  (optimize auto-integrate)
  (files s48-pessimistic-record))

(define-structures ((critical-sections/interrupt critical-sections-interface)
                    (suspenders/interrupt suspenders-interface))
  (open scheme
        cells
        interrupts
        proposals
        srfi-9                          ;define-record-type
        threads
        threads-internal
        )
  (optimize auto-integrate)
  (files s48-interrupt-suspend))

(define-structure locked-record-types/interrupt locked-record-types-interface
  (open scheme
        interrupts
        srfi-9                          ;define-record-type
        )
  (optimize auto-integrate)
  (files s48-interrupt-record))

;;;;; Parameter Selection

;++ This is pretty kludgerific.

(def critical-sections critical-sections/optimistic)
(def suspenders suspenders/optimistic)
(def locked-record-types locked-record-types/optimistic)

;; (def critical-sections critical-sections/pessimistic)
;; (def suspenders suspenders/pessimistic)
;; (def locked-record-types locked-record-types/pessimistic)

;; (def critical-sections critical-sections/interrupt)
;; (def suspenders suspenders/interrupt)
;; (def locked-record-types locked-record-types/interrupt)
