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
  (stream-cons w (sum-from (stream-cdr s) (+ (stream-car s) w))))

(define (sum-stream s)
  (stream-cons (stream-car s) (sum-from s 0)))

(define (partial-sums n s)
  (stream-ref (sum-stream s) (+ 2 n) ))
;wielokrotnosci 2
(define (2-from n)
  (stream-cons n (2-from (* 2 n))))
(define 2-stream
  (stream-cons 2 (2-from 4)))
(define (h2 n)
  (stream-ref 2-stream n))
;wielokrotnosci 3
(define (3-from n)
  (stream-cons n (3-from (* 3 n))))
(define 3-stream
  (stream-cons 3 (3-from 9)))
(define (h3 n)
  (stream-ref 3-stream n))
;wielokrotnosci 5
(define (5-from n)
  (stream-cons n (5-from (* 5 n))))
(define 5-stream
  (stream-cons 5 (5-from 25)))
(define (h5 n)
  (stream-ref 5-stream n))
;;merge
(define (merge s1 s2)
  (cond[(< (stream-car s1) (stream-car s2))
        (stream-cons (stream-car s1) (merge (stream-cdr s1) s2))]
    [(> (stream-car s1) (stream-car s2))
     (stream-cons (stream-car s2) (merge s1 (stream-cdr s2)))]
    [(= (stream-car s1) (stream-car s2))
     (stream-cons (stream-car s1) (merge (stream-cdr s1) (stream-cdr s2)))]))
;;scale

(define (scale s1 n)
  (stream-cons (* (stream-car s1) n) (scale (stream-cdr s1) n)))


;hamming
(define (mult-cycle n)
  (cond
    [(= n 2) 3]
    [(= n 3) 5]
    [(= n 5) 2]))

(define (hamming-stream)
  (define (it mult s)
    (stream-cons (stream-car s)
                (it (mult-cycle mult)
                    (merge (stream-cdr s)
                            (scale s mult)))))
  (it 2 init-stream))

(define (factorial-stream-acc)
  (define (it n last)
    (stream-cons (* last n) (it (+ n 1) (* last n))))
  (stream-cons 1 (it 1 1)))

(define init-stream
  (merge 5-stream (merge 2-stream 3-stream)))

(define (stream->list stream n)
  (if (= n 0)
      null
      (cons (stream-car stream)
            (stream->list (stream-cdr stream) (- n 1)))))