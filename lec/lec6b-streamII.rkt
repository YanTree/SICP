#lang sicp

;; import "lec6a-streamI.rkt" file
;; https://louischristopher.me/setting-up-drracket-for-sicp
(#%require "lec6a-streamI.rkt")


;;
;;; stream use

;; name: (stream-add s1 s2)
;; desc: sum of elements for s1 s2
(define (stream-add s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream (+ (stream-car s1) (stream-car s2))
                      (stream-add (stream-cdr s1)
                                  (stream-cdr s2))))))

;; name: (stream-scale s n)
;; desc: each element of stream `s' scaled by `n'
(define (stream-scale s n)
  (stream-map (lambda(element) (* element n))
              s))


;;
;;; predicate
(define (no-seven? n)
  (not (= (remainder n 7) 0)))


;;
;;; infinity stream data

;; infinity integers stream
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 0))

(define ns (stream-filter no-seven? integers))

;; Eratosthenes method
;; infinity primes start 2 stream
(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (stream-filter
           (lambda(n)
             (not (divisible? n (stream-car s))))
           (stream-cdr s)))))
(define primes (sieve (integers-starting-from 2)))

;;
;;
(define ones (cons-stream 1 ones))
(define integersn
  (cons-stream 1
               (stream-add ones integers)))

;;
;;
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (stream-add (stream-cdr fibs) fibs))))
;;
;;; stream to define data
;; (prime-sum 10 100000)