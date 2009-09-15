;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Pessimistic: Thread Suspension

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

(define (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define-record-type <suspender>
    (%make-suspender lock cell set? value)
    suspender?
  (lock suspender.lock)
  (cell suspender.cell)
  (set? suspender.set? set-suspender.set?!)
  (value suspender.value set-suspender.value!))

(define (make-suspender)
  (%make-suspender (make-lock) (make-cell (current-thread)) #f #f))

(define (suspender/lock suspender)
  (obtain-lock (suspender.lock suspender)))

(define (suspender/unlock suspender)
  (release-lock (suspender.lock suspender)))

(define (suspender/resumed? suspender)
  (suspender.set? suspender))

(define (suspender/resume suspender value)
  (set-suspender.set?! suspender #t)
  (set-suspender.value! suspender value)
  (make-ready (suspender.cell suspender)))

(define (suspender/abort suspender)
  (set-suspender.set?! suspender #t))

(define (suspender/suspend critical-token suspender)
  critical-token                        ;ignore
  (let loop ()
    (release-lock-and-block (suspender.lock suspender)
                            (suspender.cell suspender))
    (obtain-lock (suspender.lock suspender))
    (if (suspender.set? suspender)
        (let ((value (suspender.value suspender)))
          (set-suspender.value! suspender #f)
          value)
        (loop))))

(define (release-lock-and-block lock cell)
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    ;++ Bug: RELEASE-LOCK is not atomic even with interrupts disabled:
    ;++ it may let other threads run in the process of readying them.
    (release-lock lock)
    (block cell)
    (set-enabled-interrupts! interrupts)))

(define (block cell)
  (with-new-proposal (retry)
    ;; See s48-interrupt-suspend.scm for why we ignore RETRY here.
    retry                               ;ignore
    (maybe-commit-and-block cell))
  (values))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))
