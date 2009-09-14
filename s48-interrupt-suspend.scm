;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Interrupt-Based: Thread Suspension

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

(define-record-type <suspension>
    (make-suspension token composition)
    suspension?
  (token suspension.token)
  (composition suspension.composition))

(define-record-type <suspension-token>
    (%make-suspension-token locked? cell-then-thunk uid)
    suspension-token?
  (locked? suspension-token.locked? set-suspension-token.locked?!)
  (cell-then-thunk suspension-token.cell-then-thunk
                   set-suspension-token.cell-then-thunk!)
  (uid suspension-token.uid))

(define (make-suspension-token)
  (%make-suspension-token #f
                          (make-cell (current-thread))
                          (new-suspension-uid)))

(define *suspension-uid* 0)

(define (new-suspension-uid)
  (with-interrupts-inhibited
    (lambda ()
      (let ((uid *suspension-uid*))
        (set! *suspension-uid* (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (with-interrupts-inhibited
    (lambda ()
      (set! *suspension-uid* 0))))

(define (enter-critical-section procedure)
  (procedure 'CRITICAL-TOKEN))

(define (exit-critical-section critical-token continuation)
  critical-token                        ;ignore
  (continuation))

(define (suspend critical-token procedure)
  critical-token                        ;ignore
  (let ((token (make-suspension-token)))
    (set-suspension-token.locked?! token #t)
    (procedure
     (lambda (composition)
       (make-suspension token composition))
     (lambda ()
       (let ((cell (suspension-token.cell-then-thunk token)))
         (let loop ()
           ;; Atomically unlock the token and block.
           (with-interrupts-inhibited
             (lambda ()
               (set-suspension-token.locked?! token #f)
               (block cell)))
           (let spin ()
             ((with-interrupts-inhibited
                (lambda ()
                  (let ((cell-then-thunk
                         (suspension-token.cell-then-thunk token)))
                    (cond ((suspension-token.locked? token)
                           (relinquish-timeslice)
                           spin)
                          ((cell? cell-then-thunk)
                           (set-suspension-token.locked?! token #t)
                           loop)
                          (else
                           cell-then-thunk))))))))))
     (lambda (continuation)
       (continuation)))))

(define (maybe-resume suspension thunk)
  (let ((token (suspension.token suspension)))
    (let spin ()
      (let* ((interrupts (set-enabled-interrupts! no-interrupts))
             (cell-then-thunk (suspension-token.cell-then-thunk token)))
        (cond ((suspension-token.locked? token)
               (set-enabled-interrupts! interrupts)
               (relinquish-timeslice)
               (spin))
              ((cell? cell-then-thunk)
               (set-suspension-token.cell-then-thunk!
                token
                (let ((composition (suspension.composition suspension)))
                  (lambda ()
                    (composition thunk))))
               (set-enabled-interrupts! interrupts)
               (make-ready cell-then-thunk))
              (else
               (set-enabled-interrupts! interrupts)
               #f))))))

(define (with-suspension-claimed suspension if-claimed if-not-claimed)
  ((let ((token (suspension.token suspension)))
     (let spin ()
       (let* ((interrupts (set-enabled-interrupts! no-interrupts))
              (cell-then-thunk (suspension-token.cell-then-thunk token)))
         (cond ((suspension-token.locked? token)
                ;++ Should this be ALL-INTERRUPTS, rather than INTERRUPTS?
                (set-enabled-interrupts! interrupts)
                (relinquish-timeslice)
                (spin))
               ((cell? cell-then-thunk)
                (set-suspension-token.locked?! token #t)
                (set-enabled-interrupts! interrupts)
                (lambda ()
                  (if-claimed
                   (let ((composition (suspension.composition suspension)))
                     (lambda (thunk)
                       (with-interrupts-inhibited
                         (lambda ()
                           (set-suspension-token.locked?! token #f)
                           (set-suspension-token.cell-then-thunk!
                            token
                            (lambda ()
                              (composition thunk)))))
                       (make-ready cell-then-thunk)))
                   (lambda ()
                     (with-interrupts-inhibited
                       (lambda ()
                         (set-suspension-token.locked?! token #f)))))))
               (else
                (set-enabled-interrupts! interrupts)
                if-not-claimed)))))))

(define (block cell)
  (with-new-proposal (retry)
    (if (not (maybe-commit-and-block cell))
        (retry))))

(define (make-ready cell)
  (let ((thread (cell-ref cell)))
    (with-new-proposal (retry)
      (if (not (maybe-commit-and-make-ready thread))
          (retry)))))
