;;; -*- Mode: Scheme; scheme48-package: rendezvous -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48: Time Rendezvous

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

(define (after-time-rendezvous duration)
  (make-time-rendezvous (lambda () (+ (real-time) duration))))

(define (at-real-time-rendezvous time)
  (make-time-rendezvous (lambda () time)))

(define (make-time-rendezvous compute-resumption-time)

  (define (poll)
    (if (> (real-time) (compute-resumption-time))
        -1
        #f))

  (define (enable if-enabled if-disabled)
    if-disabled                         ;ignore
    (if-enabled (lambda () (values))))

  (define (block suspension if-enabled if-blocked)
    (let ((resumption-time (compute-resumption-time)))
      (if (> (real-time) resumption-time)
          (if-enabled (lambda () (values)))
          (begin
            (register-dozer!
             resumption-time
             ;++ If we lie to Scheme48 that waking this dozer has the
             ;++ effect of resuming a thread, will it get angry? -- we
             ;++ can't know, without actually resuming it, whether it
             ;++ will be resumed.
             (lambda () #t)
             (lambda ()
               (maybe-resume suspension (lambda () (values)))))
            (if-blocked)))))

  (base-rendezvous poll enable block))
