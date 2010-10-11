;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; MIT Scheme: Time Rendezvous

;;; Copyright (c) 2009--2010, Taylor R. Campbell
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

(define (after-time-rendezvous duration)
  (time-rendezvous (lambda () duration)))

(define (at-real-time-rendezvous time)
  (time-rendezvous (lambda () (- time (real-time-clock)))))

(define (time-rendezvous compute-duration)
  (delayed-rendezvous-with-nack
   (lambda (nack-rv)
     (base-time-rendezvous compute-duration nack-rv))))

(define (base-time-rendezvous compute-duration nack-rv)

  (define (poll)
    (if (positive? (compute-duration))
        #f
        -1))

  (define (enable if-enabled if-disabled)
    if-disabled                         ;ignore
    (if-enabled (lambda () (values))))

  (define (block suspension if-enabled if-blocked)
    (let ((duration (compute-duration)))
      (if (positive? duration)
          (let ((registration
                 (register-timer-event duration
                   (lambda ()
                     (maybe-resume suspension (lambda () (values)))))))
            (spawn (lambda ()
                     (synchronize nack-rv)
                     (deregister-timer-event registration)))
            (if-blocked))
          (if-enabled (lambda () (values))))))

  (base-rendezvous poll enable block))
