#lang racket

;; based on flow.ml

;; fix: (k -> v), ((k -> v), k -> v) -> k -> v
;; TODO?: allow choice of key equality function, too?
(define (fix init func #:value-equal? [value-equal? equal?])
  (define cache (make-weak-hasheq))
  (define (get key) (hash-ref! cache key (lambda () (init key))))
  (define (put! key value) (hash-set! cache key value))

  ;; frozen-set, key -> value, changed, visited-set
  ;; frozen-set: set of frozen keys
  ;; changed: boolean indicating whether any key's value changed
  ;; visited-set: set of keys we visited
  (define (compute frozen key)
    (define cached-value (get key))
    (if (set-member? frozen key)
        ;; we return (seteq key) here so that the check for (not (set-member?
        ;; key visited)), below, works. otherwise could return (seteq).
        (values cached-value #f (seteq key))
        (iterate (set-add frozen key) key cached-value)))

  (define (iterate frozen key cached-value)
    (let loop ([changed-so-far #f] [old-value cached-value])
      (define-values (new-value changed visited) (run-node frozen key))
      (unless (value-equal? old-value new-value)
        (set! changed #t)
        (put! key new-value))
      (cond
        [(not changed)
         (values old-value changed-so-far (set-add visited key))]
        ;; if we didn't depend on ourselves, there's no need to iterate.
        [(not (set-member? visited key))
         (values new-value (or changed changed-so-far) (set-add visited key))]
        [#t (loop #t new-value)])))

  ;; this is the bit where we emulate the State monad.
  (define (run-node frozen key)
    (define any-changed #f)
    (define visited-sets '())
    (define (visit key)
      (define-values (value changed visited) (compute frozen key))
      (when changed (set! any-changed #t))
      (set! frozen (set-union frozen visited))
      (set! visited-sets (cons visited visited-sets))
      value)
    (define value (func visit key))
    (values value any-changed (apply set-union (seteq) visited-sets)))

  (define finished (weak-seteq))
  (lambda (key)
    (define frozen (set-union (seteq) finished))
    (define-values (value _ visited) (compute frozen key))
    (set-union! finished visited)
    value))


;; examples
(define (func1 self n)
  (match n
    [0 (define prev (self 0))
       (if (< prev 2) (+ 1 prev) prev)]
    [n (+ 1 (self (- n 1)))]))

(define example1 (fix (const 0) func1))
