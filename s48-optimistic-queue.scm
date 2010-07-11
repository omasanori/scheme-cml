;;; -*- Mode: Scheme -*-

;;;; Scheme48: Optimistically Synchronized Queues

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

;;; This implements the same interface as Scheme48's built-in QUEUES
;;; structure.  Although both this code and Scheme48's QUEUES structure
;;; implement optimistically concurrent queues, this code is not as
;;; buggy as Scheme48's QUEUES structure (as of 2009-09-15).  For
;;; example, within a transaction, ON-QUEUE? can return two different
;;; answers for the same arguments using Scheme48's QUEUES structure,
;;; but using this code it will always give the same answer for the
;;; same arguments within a transaction.

(define-synchronized-record-type queue :queue
    (%make-queue front back)
    (back)
    queue?
  (front queue.front)
  (back queue.back set-queue.back!))

(define (make-queue)
  (let ((front (cons 'SENTINEL '())))
    (%make-queue front front)))

;;; This definition looks as though it should be equivalent to the
;;; actual one, but it's not.  The problem is not that it lacks a
;;; transaction (neither definition needs a transaction -- each one
;;; reads only once and from only one mutable location, and Scheme48
;;; guarantees individual reads to be isolated), but that someone may
;;; set the cdr of the back pair after we said that the queue was
;;; nonempty.  Although this would cause any transaction using
;;; QUEUE-EMPTY? to be restarted later on, control may never get to the
;;; end of that transaction, because it might next call DEQUEUE!, which
;;; signals an error if the queue is empty -- by testing whether the
;;; cdr of the front pair is null.  So it's better to use the same test
;;; that DEQUEUE! relies on, rather than one that looks equivalent.

;; (define (queue-empty? queue)
;;   (eq? (queue.front queue)
;;        (queue.back queue)))

(define (queue-empty? queue)
  ;; No need for a transaction: a single PROVISIONAL-CDR is always
  ;; isolated.
  (null? (provisional-cdr (queue.front queue))))

(define (queue-head queue)
  (let ((tail (provisional-cdr (queue.front queue))))
    (if (pair? tail)
        ;; Nothing sets the cars of pairs in queues, so it is safe to
        ;; use CAR, rather than PROVISIONAL-CAR, here.
        (car tail)
        (error "Empty queue:" queue queue-head))))

;;; Every destructive operation on a queue Q leaves the following true:
;;;
;;;   (NULL? (PROVISIONAL-CDR (QUEUE.BACK Q)))
;;;
;;; The only relevant SET-CDR!s arise in COPY-CDR!, NULLIFY-CDR!, and
;;; ENQUEUE!, which all enforce this property.  COPY-CDR! and
;;; NULLIFY-CDR! are internal routines called only in transactions.
;;;
;;; Scheme48's optimistic concurrency does not guarantee that this
;;; property holds on entry to a queue operation, however, particularly
;;; the first of a transaction.  Fortunately, ENQUEUE! is the only
;;; operation that reads QUEUE.BACK, and always leaves the queue in a
;;; consistent state for the transaction, even if the transaction must
;;; be repeated because another thread invalidated its read of the
;;; QUEUE.BACK.  This way, every transaction always sees a consistent
;;; state of each queue, whether or not the transaction succeeds.

(define (copy-cdr! target-pair source-pair queue)
  (let ((d (provisional-cdr source-pair)))
    (provisional-set-cdr! target-pair d)
    (if (null? d)
        (set-queue.back! queue target-pair))))

(define (nullify-cdr! pair queue)
  (provisional-set-cdr! pair '())
  (set-queue.back! queue pair))

(define (enqueue! queue element)
  (let ((pair (cons element '())))
    (call-ensuring-atomicity!
      (lambda ()
        (provisional-set-cdr! (queue.back queue) pair)
        (set-queue.back! queue pair)))))

(define (dequeue! queue)
  ((call-ensuring-atomicity
     (let ((front (queue.front queue)))
       (lambda ()
         (let ((pair (provisional-cdr front)))
           (if (pair? pair)
               (begin
                 (copy-cdr! front pair queue)
                 (lambda ()
                   (car pair)))
               (lambda ()
                 (error "Empty queue:" queue dequeue!)))))))))

;; (define (dequeue! queue)
;;   (car
;;    (or (call-ensuring-atomicity
;;          (let ((front (queue.front queue)))
;;            (lambda ()
;;              (let ((pair (provisional-cdr front)))
;;                (and (pair? pair)
;;                     (begin (copy-cdr! front pair queue)
;;                            pair))))))
;;        (error "Empty queue:" queue dequeue!))))

(define (maybe-dequeue! queue)
  (call-ensuring-atomicity
    (let ((front (queue.front queue)))
      (lambda ()
        (let ((pair (provisional-cdr front)))
          (if (pair? pair)
              (begin (copy-cdr! front pair queue)
                     (car pair))
              #f))))))

;;; Scheme48's big/queue.scm comments that MAYBE-DEQUEUE! is necessary
;;; to avoid a race condition, but this is not the case: a transaction,
;;; QUEUE-EMPTY?, and DEQUEUE! suffice, and avoid the #F bug to boot.

;; (define (maybe-dequeue! queue)
;;   (call-ensuring-atomicity
;;     (lambda ()
;;       (if (queue-empty? queue)
;;           #f
;;           (dequeue! queue)))))

(define (empty-queue! queue)
  (call-ensuring-atomicity!
    (let ((front (queue.front queue)))
      (lambda ()
        (nullify-cdr! front queue)))))

(define (on-queue? queue element)
  (call-ensuring-atomicity
    (let ((front (queue.front queue)))
      (lambda ()
        (let loop ((pair front))
          (let ((tail (provisional-cdr pair)))
            (and (pair? tail)
                 (or (eq? element (car tail))
                     (loop tail)))))))))

(define (delete-from-queue! queue element)
  (delete-from-queue-if! queue (lambda (element*) (eq? element* element))))

(define (delete-from-queue-if! queue predicate)
  (call-ensuring-atomicity
    (let ((front (queue.front queue)))
      (lambda ()
        (let loop ((pair front))
          (let ((tail (provisional-cdr pair)))
            (if (pair? tail)
                (if (predicate (car tail))
                    (begin (copy-cdr! pair tail queue) #t)
                    (loop tail))
                #f)))))))

(define (queue-length queue)
  (call-ensuring-atomicity
    (let ((front (queue.front queue)))
      (lambda ()
        (let loop ((pair front) (length 0))
          (let ((tail (provisional-cdr pair)))
            (if (pair? tail)
                (loop tail (+ length 1))
                length)))))))

(define (queue->list queue)
  (reverse
   (call-ensuring-atomicity
     (let ((front (queue.front queue)))
       (lambda ()
         (let loop ((pair front) (list '()))
           (let ((tail (provisional-cdr pair)))
             (if (pair? tail)
                 (loop tail (cons (car tail) list))
                 list))))))))

;;; Which of these exhibits the intended behaviour of LIST->QUEUE?
;;; Generally, you will probably want to do something like
;;;
;;;   (nonprovisional-list->queue (provisional-cell-ref list-cell))
;;;
;;; rather than use PROVISIONAL-LIST->QUEUE.

(define (nonprovisional-list->queue list)
  (let ((front (cons 'SENTINEL '())))
    (let loop ((back front) (list list))
      (if (pair? list)
          (let ((back* (cons (car list) '())))
            ;; Using SET-CDR! here is safe: these are strictly local
            ;; effects, not visible to any thread but the current one
            ;; until LIST->QUEUE returns.  But CAR and CDR mean you
            ;; can't pass in a list undergoing concurrent update.
            (set-cdr! back back*)
            (loop back* (cdr list)))
          (%make-queue front back)))))

(define (provisional-list->queue list)
  (call-ensuring-atomicity
    (lambda ()
      ;; Creating FRONT inside the transaction rather than outside is
      ;; not strictly necessary: when we repeat the transaction, we
      ;; shall clobber its cdr anyway.  But this feels less sketchy.
      (let ((front (cons 'SENTINEL '())))
        (let loop ((back front) (list list))
          (if (pair? list)
              (let ((back* (cons (provisional-car list) '())))
                ;; Using SET-CDR! here is safe, as above.  But we use
                ;; PROVISIONAL-CxR instead of CxR, to make it safe to
                ;; use PROVISIONAL-LIST->QUEUE on a list undergoing
                ;; concurrent update.
                (set-cdr! back back*)
                (loop back* (provisional-cdr list)))
              (%make-queue front back)))))))

(define (list->queue list)
  (provisional-list->queue list))
