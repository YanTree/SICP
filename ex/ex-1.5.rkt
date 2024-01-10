#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; umcomment below code to view action
;; (test 0 (p))

;; answer
;; 若为应用序,调用 (test 0 (p)) 而先求值 operands 里的 0 和 (p),求值 (p) 时程序就进入循环,不断的调用 (p),此刻程序表现为陷入循环而宕机了一样。
;; 若为正则序,所有东西都需要先展开,在遇到 primitive procedure 时再做实际的 arguments 逐一映射到 parameters 的代换步骤(仅代换无求值呃,请千万留意),test procedure 先需要 x 的值,所以先求得 x 值,值为 0,满足程序的判定条件,输出 0。
(define answer "若为应用序,调用 (test 0 (p)) 而先求值 operands 里的 0 和 (p),求值 (p) 时程序就进入循环,不断的调用 (p),此刻程序表现为陷入循环而宕机了一样。\n若为正则序,所有东西都需要先展开,在遇到 primitive procedure 时再做实际的 arguments 逐一映射到 parameters 的代换步骤(仅代换无求值呃,请千万留意),test procedure 先需要 x 的值,所以先求得 x 值,值为 0,满足程序的判定条件,输出 0")
(display answer)