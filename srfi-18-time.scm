;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; SRFI 18: Time Rendezvous

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

(define (after-time-rendezvous duration-in-milliseconds)

  (define (poll)
    (if (positive? duration-in-milliseconds)
        #f
        -1))

  (define (enable if-enabled if-disabled)
    if-disabled                         ;ignore
    (if-enabled (lambda () (values))))

  (define (block suspension if-enabled if-blocked)
    if-enabled                          ;ignore
    (spawn-timer suspension (/ duration-in-milliseconds 1000))
    (if-blocked))

  (base-rendezvous poll enable block))

(define (at-real-time-rendezvous real-time-in-milliseconds)
  (let ((real-time-in-seconds (/ real-time-in-milliseconds 1000)))

    (define (poll)
      (if (< (time->seconds (current-time)) real-time-in-seconds)
          -1
          #f))

    (define (enable if-enabled if-disabled)
      if-disabled                       ;ignore
      (if-enabled (lambda () (values))))

    (define (block suspension if-enabled if-blocked)
      ;; Some small amount of time may have passed.  Is this worth
      ;; checking?
      (if (< (time->seconds (current-time)) real-time-in-seconds)
          (begin
            (spawn-timer suspension (seconds->time real-time-in-seconds))
            (if-blocked))
          (if-enabled (lambda () (values)))))

    (base-rendezvous poll enable block)))

(define (spawn-timer suspension timeout)
  ;; It would be nice if we could terminate this thread on negative
  ;; acknowledgement, but the base rendezvous layer doesn't have any
  ;; way to do that at the moment.
  (thread-start!
   (make-thread
    (lambda ()
      (thread-sleep! timeout)
      (maybe-resume suspension (lambda () (values)))))))
