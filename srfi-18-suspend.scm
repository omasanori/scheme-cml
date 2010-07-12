;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; SRFI 18: Thread Suspension

;;; Copyright (c) 2009, 2010, Taylor R. Campbell
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

(define (enter-critical-section body)
  (body 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define-record-type <suspender>
    (%make-suspender mutex condvar state)
    suspender?
  (mutex suspender.mutex)
  (condvar suspender.condvar)
  ;; Possible values of the state field:
  ;;   #f -> aborted or already used
  ;;   <thread> -> waiting, if alive
  ;;   (<value>) -> resumed with <value>
  (state suspender.state set-suspender.state!))

(define (make-suspender)
  (%make-suspender (make-mutex) (make-condition-variable) (current-thread)))

(define (assert-suspender-lock suspender)
  (let ((me (current-thread)))
    (if (not (eq? me (mutex-state (suspender.mutex suspender))))
        (error "I don't have this suspender locked:" suspender me))))

(define (live-thread? object)
  (and (thread? object)
       (thread-alive? object)))

;;; Plagiarized from the SRFI 18 document.

(define (thread-alive? thread)
  (let ((mutex (make-mutex)))
    (mutex-lock! mutex #f thread)
    (let ((state (mutex-state mutex)))
      (mutex-unlock! mutex)
      (eq? state thread))))

;;; Important: SUSPENDER/LOCK locks the suspender even if it was locked
;;; by a thread that has terminated.  Thus, we must keep the suspender
;;; in a consistent state even while it is locked.

(define (suspender/lock suspender)
  (let ((me (current-thread)))
    (if (eq? me (mutex-state (suspender.mutex suspender)))
        (error "I already have this suspender locked:" suspender me)))
  (with-exception-handler (lambda (exception)
                            (if (abandoned-mutex-exception? exception)
                                #t
                                (raise exception)))
    (lambda ()
      (mutex-lock! (suspender.mutex suspender)))))

(define (suspender/unlock suspender)
  (assert-suspender-lock suspender)
  (mutex-unlock! (suspender.mutex suspender)))

(define (suspender/resumed? suspender)
  (assert-suspender-lock suspender)
  (not (live-thread? (suspender.state suspender))))

(define (suspender/resume suspender value)
  ;; Whether we signal or broadcast is immaterial: at most one thread
  ;; will ever be waiting on the condition variable.  But the order of
  ;; actions here is important.  It is safe to signal the suspended
  ;; thread before we have updated the state -- that thread won't try
  ;; to read the state until we unlock the suspender.  But if we
  ;; updated the state first and were interrupted before we got a
  ;; chance to signal the suspended thread, then the suspender would be
  ;; set, and the suspended thread would never know.
  (condition-variable-broadcast! (suspender.condvar suspender))
  (set-suspender.state! suspender (list value)))

(define (suspender/abort suspender)
  (set-suspender.state! suspender #f))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  (assert-suspender-lock suspender)
  (let loop ()
    (mutex-unlock! (suspender.mutex suspender)
                   (suspender.condvar suspender))
    (mutex-lock! (suspender.mutex suspender))
    (let ((state (suspender.state suspender)))
      (cond ((pair? state) (set-suspender.state! suspender #f) (car state))
            ((eq? state (current-thread)) (loop))
            (else (error "Suspender reused or aborted:" suspender))))))

