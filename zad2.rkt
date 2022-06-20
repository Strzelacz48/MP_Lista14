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
(define (prime2? n)
  (define (ok a)
    (define pomi (stream-ref prime-stream a))
    (if (< n (* pomi pomi))
        #t
        (if (eq? 0 (modulo n pomi))
            #f
            (ok (+ a 1)))))
  (if (< n 2) #f (ok 0)))

(define (next-prime n)
  (if (prime2? n) n (next-prime (+ 1 n)))
  )

(define (prime-from n)
  (define pom (next-prime n))
  (stream-cons pom (prime-from (+ 1 pom))))

(define prime-stream
  (stream-cons 2 (prime-from 3)))

(define (h n)
  (stream-ref prime-stream n))

;

(define (sum-from s w)
  (stream-cons w (sum-from (stream-cdr s) (+ (stream-car s) w))
               ))

(define (sum-stream s)
  (stream-cons (stream-car s) (sum-from s 0)))

(define (partial-sums n s)
  (stream-ref (sum-stream s) (+ 2 n) ))

;(define (b n)
 ; (map h (enumerate-interval 0 n)))