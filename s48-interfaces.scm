;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48: Interface Definitions

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

(define-interface rendezvous-interface
  (export
    after-time-rendezvous
    at-real-time-rendezvous
    choosing-rendezvous
    choosing-rendezvous*
    delayed-rendezvous
    delayed-rendezvous-with-nack
    map-rendezvous
    poll
    quiescent-rendezvous
    (rendezvous-case :syntax)
    rendezvous?
    synchronize
    synchronize-chosen-rendezvous
    synchronize-chosen-rendezvous/timeout
    synchronize/timeout
    thunk-rendezvous
    values-rendezvous
    ))

(define-interface rendezvous-channels-interface
  (export
    channel-receive
    channel-receive-rendezvous
    channel-send
    channel-send-rendezvous
    channel?
    make-channel
    ))

(define-interface rendezvous-mailboxes-interface
  (export
    mailbox-receive
    mailbox-receive-rendezvous
    mailbox-send
    mailbox?
    make-mailbox
    ))

(define-interface rendezvous-placeholders-interface
  (export
    make-placeholder
    placeholder-value
    placeholder-value-rendezvous
    placeholder?
    set-placeholder!
    ))

(define-interface rendezvous-semaphores-interface
  (export
    make-semaphore
    semaphore-acquire
    semaphore-acquire-rendezvous
    semaphore-release
    semaphore-value
    semaphore?
    ))

;;;; Parameters

(define-interface critical-sections-interface
  (export
    enter-critical-section
    exit-critical-section
    ))

(define-interface suspenders-interface
  (export
    make-suspender
    suspender/abort
    suspender/lock
    suspender/resume
    suspender/resumed?
    suspender/suspend
    suspender/unlock
    suspender?
    ))

(define-interface locked-record-types-interface
  (export
    (define-locked-record-type :syntax)
    ))

;;;; Internals

(define-interface primitive-rendezvous-interface
  (export
    make-prv
    map-prv
    primitive-poll
    primitive-synchronize
    prv?
    ))

(define-interface resumption-interface
  (export
    with-suspension-claimed
    maybe-resume
    ))

;;; This is here because compound interfaces cannot have forward
;;; references.

(define-interface rendezvous-builder-interface
  (compound-interface
   (export base-rendezvous)
   critical-sections-interface
   resumption-interface
   ))
