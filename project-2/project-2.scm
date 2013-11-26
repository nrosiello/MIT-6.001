;; Project 2
;;
;; Problem 1: Extract entry function
(define (extract-entry play game)
  (if (equal? (caar game) play)
    (car game)
    (extract-entry play (cdr game))))
