;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project 5: the meta-circular evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "meval.scm")
(load "syntax.scm")
(load "environment.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 1: exploring meval and adding new primitive procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; explore using meval
(m-eval-global '(+ 4 5))
(m-eval-global '(cons 'a 'b))
(m-eval-global '(< 1 2))

;; test new primitive procedures
(test-equal (m-eval-global '(* 2 3)) 6)
(test-equal (m-eval-global '(/ 10 5)) 2)
(test-equal (m-eval-global '(cadr '(a b c))) 'b)
(test-equal (m-eval-global '(cddr '(a b c))) '(c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 2: permit if expressions to lack an alternative expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initially, meval would produce an error if an if statement lacked
;; an alternative expression

;; test the modified if statement
(test-equal (m-eval-global '(if (< 3 4) 'a 'b)) 'a)
(test-equal (m-eval-global '(if (< 3 4) (+ 1 2))) 3)
(test-equal (m-eval-global '(if (< 5 4) (+ 1 2))) #f)
(test-equal (m-eval-global '(if #f #t (* 2 6))) 12)
