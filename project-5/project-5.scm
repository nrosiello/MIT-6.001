;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project 5: the meta-circular evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "meval.scm")
(load "syntax.scm")
(load "environment.scm")
(load "../common/test.scm")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 3: addition of the do-while loop special form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests for the do-while selectors
(define do-while-1 '(do (+ 1 3)
                       'a
                       'b
                       while (< 3 4)))
(test-equal (do-while-exps do-while-1) '((+ 1 3) 'a 'b))
(test-equal (do-while-predicate do-while-1) '(< 3 4))

;; tests for the do-while loop
(m-eval-global '(define x '()))
(test-equal (m-eval-global '(do (set! x (cons '* x))
              (+ 1 4)
              (* 3 4)
              while (< (length x) 3))) 'done)
(test-equal (m-eval-global 'x) '(* * *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 4: the let* special form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; confirm regular let behavior
(m-eval-global '(define i 1))
(define let-ex-cdr '(((i 3)
                      (j (* 5 i)))
                      (+ 4 5)
                      (list i j)))
(define let-ex (cons 'let let-ex-cdr))
(define let*-ex (cons 'let* let-ex-cdr))
(test-equal (m-eval-global let-ex) '(3 5)) 

;; identify a let* expression
(test-equal (let*? let*-ex) #t)
(test-equal (let*? let-ex) #f)

;; extract the parts of a let expression
(test-equal (let*-expr let*-ex) '((+ 4 5)
                                  (list i j)))
(test-equal (let*-bound-variables let*-ex) '(i j))
(test-equal (let*-values let*-ex) '(3 (* 5 i)))

;; expression to access the redefined value of i
(test-equal (m-eval-global let*-ex) '(3 15)) 

;; let* with no bound variables
(test-equal (m-eval-global '(let* () (+ 3 4) (- 4 2))) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 5: unset! special form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper function to extract unset! variable
(test-equal (unset!-variable '(unset! x)) 'x)

;; binding variable functions to manage the environment state
(define has-been-set-binding '(x 3 2 1))
(unset-binding-value! has-been-set-binding)
(test-equal has-been-set-binding '(x 2 1))

(define has-not-been-set-binding '(x 5))
(unset-binding-value! has-not-been-set-binding)
(test-equal has-not-been-set-binding '(x 5))

;; test the unset! function
(m-eval-global '(begin
                  (define z 4)
                  (set! z 5)
                  (set! z 6)
                  (unset! z)))
(test-equal (m-eval-global 'z) 5)

(m-eval-global '(unset! z))
(test-equal (m-eval-global 'z) 4)

(m-eval-global '(unset! z))
(test-equal (m-eval-global 'z) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 6: de-sugared do-while loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; since this only re-defines the implementation of do-while evaluation,
;; the tests from exercise 3 are still valid

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 7: and/or special forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests for helper functions
(test-equal (pop-predicate '(and 3 2 1)) '(and 2 1))
(test-equal (first-predicate '(and 3 2 1)) '3)

(test-equal (no-predicates? '(and 3 2 1)) #f)
(test-equal (no-predicates? '(and)) #t)

(test-equal (one-predicate? '(and 3 2 1)) #f)
(test-equal (one-predicate? '(and)) #f)
(test-equal (one-predicate? '(and (+ 3 2))) #t)

;; test or
(test-equal (m-eval-global '(or)) #f)
(test-equal (m-eval-global '(or #f)) #f)
(test-equal (m-eval-global '(or #f #t)) #t)
(test-equal (m-eval-global '(or #f (+ 1 4))) 5)

;; test and
(test-equal (m-eval-global '(and)) #t)
(test-equal (m-eval-global '(and 1)) 1)
(test-equal (m-eval-global '(and 1 #f)) #f)
(test-equal (m-eval-global '(and 1 2 (* 3 2))) 6)
(test-equal (m-eval-global '(and 1 2 3 #f 4 5)) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 8: de-sugaring the case statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define case-ex '(case 'message
                   (('TYPE1 'TYPE2 'TYPE3) (* 3 2) 
                                (+ 1 1))
                   (('message) (+ 1 2))
                   (else (+ 1 3))))

(define case-ex-else '(case 'a
                   (('b 'c 'd) "hello")
                   (else "world")))

;; test the functions to extract parts of case expressions
(test-equal (extract-case-val case-ex) '(quote message))
(test-equal (extract-case-exprs case-ex) 
  '((((quote type1) (quote type2) (quote type3)) (* 3 2) (+ 1 1))
    (((quote message)) (+ 1 2)) 
    (else (+ 1 3))))

;; test identification of a case expression
(test-equal (case? case-ex) #t)
(test-equal (case? '(and 3 4)) #f)

;; test the creation of the cond predicate statements
(test-equal (case-var->cond-pred '(x y z) 'a)
            '(or (eq? x a) (eq? y a) (eq? z a)))
(test-equal (case-var->cond-pred 'else 'a)
            'else)

;; test the case function conversion to a cond statement and evaluation
(test-equal (m-eval-global case-ex) 3)
(test-equal (m-eval-global case-ex-else) "world")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exercise 9: avoiding name capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test simple name list creation
(test-equal (names-used-in "hello") '())
(test-equal (names-used-in '(x)) '(x))
(test-equal (names-used-in '('a)) '())
(test-equal (names-used-in '(or a b)) '(b a))
(test-equal (names-used-in '(and a b)) '(b a))
(test-equal (names-used-in '(unset! x)) '(x))

;; test compound expression name creation
(test-equal (names-used-in
              '(lambda (x y) (+ 2 3)))
            '(+ y x))
(test-equal (names-used-in
  '(let* ((x 4)
  	 (y val))
     (if (or z (not z))
     	(+ x y)
	    7)))
            '(+ not z val y x))
(test-equal (names-used-in
  '(do (display (* loop x x))
       (set! x (+ x 1))
     while (< x n)))
            '(+ loop * display n x <))
(test-equal (names-used-in 
  '(case a
     ((b c) d)
     (else e)))
            '(e d c a b eq?))

;; test creation of a fresh-symbol given a name list
(test-equal (fresh-symbol '(+ not z val y x))
            (quote +notzvalyxunused))
