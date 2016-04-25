#lang racket

(provide define/fix)

;; this should only work if the function is monotone in itself! I think...
;;
;; or if it's not really recursive - if there's some order you can put on its
;; points such that there are no cycles, or such that all cycles are monotone.
;;
;; "ALL CYCLES ARE MONOTONE" - now there's a slogan.
;; "no vicious circles"
(define-syntax-rule (define/fix (function argument) #:bottom bottom-expr
                      body ...)
  (begin
    (define done   (mutable-seteq))
    (define cache  (make-weak-hasheq))
    (define bottom bottom-expr)
    (define (get x) (hash-ref cache x (lambda () bottom)))
    (define (done? x) (set-member? done x))

    (define (function x)
      (begin0 (if (done? x)
                  (begin0 (get x)
                    (printf "already computed ~s\n" x))
                  (compute x))
        (printf "done: ~s\n" (set->list done))
        (printf "cache: ~s\n" (hash->list cache))))

    (define (compute x)
      (printf "computing ~s\n" x)
      (let loop ([old-value bottom])
        (define-values (new-value done?) (compute-next-value x))
        (cond
          [done? new-value]
          [(equal? old-value new-value)
           (set-add! done x)
           new-value]
          [#t (loop new-value)])))

    (define (compute-next-value x)
      (printf " iterating ~s, previous value ~s\n" x (get x))
      (define visited (mutable-seteq))

      ;; visit and compute return (values x-val x-done?)
      ;; visit tests for early exit conditions and otherwise calls compute.
      (define (visit x)
        (cond
          [(done? x) (printf "  done: ~s = ~s\n" x (get x)) (values (get x) #t)]
          [(set-member? visited x)
           (printf "  already visited ~s = ~s\n" x (get x)) (values (get x) #f)]
          [#t (compute x)]))

      (define (compute x)
        (printf "  computing ~s\n" x)
        (set-add! visited x)
        (define x-done #t)
        ;; this is kind of a hack. want a monad here, really.
        (define (function y)
          (define-values (y-val y-done) (visit y))
          (unless y-done (set! x-done #f))
          y-val)
        (define new-value (let ([argument x]) body ...))
        (when x-done (set-add! done x))
        (hash-set! cache x new-value)
        (values new-value x-done))

      (visit x))))

(define/fix (foo k) #:bottom #f
  (match k
    [0 "hello"]
    [1 (foo 0)]
    [2 (foo 1)]))

;; NB: I'm not sure this function is monotone in itself
(define/fix (bar k) #:bottom 0
  (match k
    [0 (let ([self (bar 0)])
         (if (< self 3) (+ 1 self) self))]
    [(? even? n)
     (if (even? (bar (- n 1)))
         (* 2 (bar (/ n 2)))
         (+ 1 (bar (- n 1))))]
    [(? odd? n)
     (bar (- n 1))]))

;; TODO: computing (bar 2) computes (bar 2) & (bar 1) three times, because (bar
;; 0) takes 3 iterations to settle. but (bar 0) doesn't depend on (bar 1) or
;; (bar 2); is there a natural algorithm that iterates (bar 0) until no change?

;; Yes, I think there is.
;;
;; Use the Writer monad again, but this time produce "read-sets" of nodes whose
;; cached values we used, including only nodes which we are currently trying to
;; compute recursively - so keep a stack-set of nodes being computed, and
;; whenever you read from one of them, produce it in your read-from set.
;;
;; what about nodes which we visited, but which aren't done, but which aren't
;; nodes in our stack-set? can that happen? I think not, if we guarantee running
;; SCCs to completion, but I'm not sure. and I'm not sure we guarantee that!
;;
;; When you finish computing a node, take the set of nodes depended on. if it's
;; empty, we're done (I think). if it contains only yourself, we've just
;; finished a SCC (strongly-connected component), and can iterate to fixed-point
;; freely! otherwise, remove yourself and report the set on upwards.
;;
;; is there any way for us to have finished an SCC and have it not be empty?
