#lang sicp

(define square
  (lambda (n) (* n n)))

(define sum-square
  (lambda (a b)
    (+ (square a) (square b))))


(define sum-of-largest
  (lambda (a b c)
    (sum-square (max a b)
                (max (min a b) c))))

;; test
(sum-of-largest 5 6 2)
(sum-of-largest 8 4 2)
(sum-of-largest 4 3 6)