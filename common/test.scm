;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple testing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-option 'format)

(define (test-equal a b)
  (if (not (equal? a b))
    (format #t "Test failed: ~A not equal to ~A\n" a b)))
