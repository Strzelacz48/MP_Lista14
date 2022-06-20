#lang racket

;; processing data sequences with lists --------------------

; (second-prime-in-interval 10000 5000000)
;
;(define (enumerate-interval a b)
;  (if (> a b)
;      '()
;      (cons a (enumerate-interval (+ a 1) b))))
;
;(define (square x)
;  (* x x))
;
;(define (smallest-divisor n)
;  (find-divisor n 2))
;
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))
;
;(define (divides? a b)
;  (= (remainder b a) 0))
;
;(define (prime? n)
;  (= n (smallest-divisor n)))

;; streams aka lazy lists ---------------------------------

;; delay and force

(define-syntax-rule
  (stream-cons v s)
  (cons v (delay s)))

(define stream-car car)

(define (stream-cdr s)
  (force (cdr s)))

(define stream-null null)
(define stream-null? null?)

;; operations on streams

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter p s)
  (if (stream-null? s)
      stream-null
      (if (p (stream-car s))
          (stream-cons (stream-car s)
                       (stream-filter p (stream-cdr s)))
          (stream-filter p (stream-cdr s)))))

;(define (stream-enumerate-interval a b)
;  (if (> a b)
;      stream-null
;      (stream-cons a (stream-enumerate-interval (+ a 1) b))))
;
;(define (stream-second-prime-in-interval a b)
;  (stream-car
;   (stream-cdr
;    (stream-filter prime?
;                   (stream-enumerate-interval a b)))))



(define (next-fact n fact)
  (* n fact)
  )

(define (fact-from n fact)
  (define pom (next-fact (+ n 1) fact))
  (stream-cons fact (next-fact (+ n 1) fact)))

(define fact-stream
  (stream-cons 1 (fact-from 1 1)))

(define (h n)
  (stream-ref fact-stream n))

;(define (b n)
 ; (map h (enumerate-interval 0 n)))