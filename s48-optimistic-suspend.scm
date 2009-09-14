;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Scheme48, Optimistic: Thread Suspension

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

(define-synchronized-record-type suspension-token <suspension-token>
    (%make-suspension-token locked? cell-then-thunk uid)
    (locked? cell-then-thunk)
    suspension-token?
  (locked? suspension-token.locked? set-suspension-token.locked?!)
  (cell-then-thunk suspension-token.cell-then-thunk
                   set-suspension-token.cell-then-thunk!)
  (uid suspension-token.uid))

(define (make-suspension-token)
  (%make-suspension-token #f
                          (make-cell (current-thread))
                          (new-suspension-uid)))

(define suspension-uid-cell (make-cell 0))

(define (new-suspension-uid)
  (call-ensuring-atomicity
    (lambda ()
      (let ((uid (provisional-cell-ref suspension-uid-cell)))
        (provisional-cell-set! suspension-uid-cell (+ uid 1))
        uid))))

(define (reset-suspension-uid)
  (call-ensuring-atomicity
    (lambda ()
      (provisional-cell-set! suspension-uid-cell 0))))

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
           (with-new-proposal (retry)
             (set-suspension-token.locked?! token #f)
             (if (not (maybe-commit-and-block cell))
                 (retry)))
           ((with-new-proposal (retry)
              (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
                (cond ((suspension-token.locked? token)
                       ;; Spin.
                       (relinquish-timeslice)
                       (retry))
                      ((cell? cell-then-thunk)
                       (set-suspension-token.locked?! token #t)
                       (if (maybe-commit) loop (retry)))
                      (else
                       (if (maybe-commit) cell-then-thunk (retry))))))))))
     (lambda (continuation)
       (continuation)))))

(define (maybe-resume suspension thunk)
  (error-if-current-proposal)
  (let ((token (suspension.token suspension)))
    (with-new-proposal (retry)
      ;; Read CELL-THEN-THUNK first, because that is set only once: if
      ;; it has been set to a thunk, then the lock state won't ever
      ;; change.  If we read the lock state first, but after we read
      ;; lock state someone else claims the suspension without resuming
      ;; it yet, we might think that we can claim it when we can't.
      (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
        (cond ((suspension-token.locked? token)
               ;; Spin the transaction.  We don't need to invalidate
               ;; the current proposal because we can elide the attempt
               ;; to commit it altogether.
               (relinquish-timeslice)
               (retry))
              ((cell? cell-then-thunk)
               (set-suspension-token.cell-then-thunk!
                token
                (let ((composition (suspension.composition suspension)))
                  (lambda ()
                    (composition thunk))))
               (if (maybe-commit-and-make-ready (cell-ref cell-then-thunk))
                   #t
                   (retry)))
              (else
               (if (maybe-commit)
                   #f
                   (retry))))))))

;;; Multiprocessor improvement to all this nonsense: Record the owner
;;; of the suspension token's lock: if it is on another processor,
;;; don't relinquish our time slice on this processor; just spin.

(define (with-suspension-claimed suspension if-claimed if-not-claimed)
  (let ((token (suspension.token suspension)))
    ((call-ensuring-atomicity
       (lambda ()
         (let ((cell-then-thunk (suspension-token.cell-then-thunk token)))
           (cond ((suspension-token.locked? token)
                  ;; Someone else has claimed this suspension, but has
                  ;; not yet decided whether to use it.  Spin the
                  ;; transaction until a decision is made.
                  (invalidate-current-proposal!)
                  (relinquish-timeslice)
                  ;; Arbitrarily say that we couldn't claim it.
                  ;; Whatever is computed in the transaction later, it
                  ;; will fail to commit its proposal anyway, and have
                  ;; to restart.  However, it would be nice if
                  ;; INVALIDATE-CURRENT-PROPOSAL! actually threw out of
                  ;; the transaction so that no such arbitrary decision
                  ;; would be necessary.
                  if-not-claimed)
                 ((cell? cell-then-thunk)
                  (set-suspension-token.locked?! token #t)
                  (lambda ()
                    (if-claimed
                     (let ((composition (suspension.composition suspension))
                           (thread (cell-ref cell-then-thunk)))
                       (lambda (thunk)
                         (error-if-current-proposal)
                         (with-new-proposal (retry)
                           (set-suspension-token.locked?! token #f)
                           (set-suspension-token.cell-then-thunk!
                            token
                            (lambda ()
                              (composition thunk)))
                           (if (not (maybe-commit-and-make-ready thread))
                               (retry)))))
                     (lambda ()
                       (set-suspension-token.locked?! token #f)))))
                 (else
                  if-not-claimed))))))))

(define (error-if-current-proposal)
  (if (current-proposal)
      (error "I can't resume a process while I'm in a transaction.")))
