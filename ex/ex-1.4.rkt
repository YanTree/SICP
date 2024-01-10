#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; answer
;; 若 b > 0,则为 (+ a b),若 b < 0,则为 (- a b);所以该程序功能:a 加上 b 的绝对值
(define answer "若 b > 0，则为 (+ a b)，若 b < 0，则为 (- a b)；所以该程序功能：a 加上 b 的绝对值")
(display answer)