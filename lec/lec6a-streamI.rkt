#lang sicp

;;
;;; math

;; name: (square n)
;; desc: evaluate for n * n
(define (square n) (* n n))

;; name: (smallest-divisor n)
;; desc: smallest divisor of number n
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

;; name: (divides? a b)
;; desc: is a divide b? another language is a%b
(define (divides? a b)
  (= (remainder b a) 0))

;; name: (prime? n)
;; desc: is n prime?
(define (prime? n)
  (= n (smallest-divisor n)))


;;
;;; stream

(define stream-null? null?)
(define the-empty-stream '())


;;
;;; stream as list present

;; stream constructor
;;(define cons-stream cons)

;; stream selectors
;;(define stream-car car)
;;(define stream-cdr cdr)


;;
;;; stream not list present

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; (define (delay s-exp) (lambda () s-exp))
;; This is really means for `delay', isn't like above code. More info at below link
;; https://www.lvguowei.me/post/sicp-goodness-stream-1/
(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc (lambda () exp)))))

(define (force delay-obj)
  (delay-obj))

;; constructor
;; (define (cons-stream a b) (cons a (delay b)))
;; This is really means for `cons-stream', almost same reson for `delay'
;; https://www.lvguowei.me/post/sicp-goodness-stream-1/
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

;; selectors
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))


;;
;;; stream use

;; name:
;; desc:
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; name:
;; desc:
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;; name:
;; desc:
(define (stream-filter pred? s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred? (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred? (stream-cdr s))))
        (else (stream-filter pred? (stream-cdr s)))))

;; name:
;; desc:
(define (stream-accumulate combiner init-val s)
  (if (stream-null? s)
      init-val
      (combiner
       (stream-car s)
       (stream-accumulate
        combiner
        init-val
        (stream-cdr s)))))

;; name:
;; desc:
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

;; name；
;; desc:
(define (stream-display s)
  (stream-for-each (lambda (element) (newline) (display element))
                   s))


;; name:
;; desc:
(define (stream-enumerate-interval start end)
  (if (> start end)
      the-empty-stream
      (cons-stream
       start
       (stream-enumerate-interval (+ start 1) end))))


;;
;;; stream data

;; name:
;; desc:
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 0))


;;
;;; traditional style procedure

;; sum of all primes at range [a, b]
(define (prime-sum a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
           (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

;; nth prime at range [a, b]
(define (prime-ref a b n)
  (define (iter test prime count)
    (cond ((> test b) 'none)
          ((= count n) prime)
          ((prime? test)
           (iter (+ test 1) test (+ count 1)))
          (else (iter (+ test 1) prime count))))
  (iter a 0 -1))


;;
;;; stream style procedure 

;; sum of all primes at range [a, b]
(define (stream-prime-sum a b)
  (stream-accumulate +
                     0
                     (stream-filter prime?
                                    (stream-enumerate-interval a b))))

;; nth prime at range [a, b]
(define (stream-prime-ref a b n)
  (stream-ref (stream-filter prime?
                             (stream-enumerate-interval a b))
              n))
