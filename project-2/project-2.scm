;; project 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 1: extract entry function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extract-entry play game)
  (if (equal? (caar game) play)
    (car game)
    (extract-entry play (cdr game))))

; test problem 1:
(define a-play (make-play "c" "d"))
(extract-entry a-play *game-association-list*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 2: performance of different strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; patsy vs. eye-for-eye
(play-loop PATSY EYE-FOR-EYE)
; score:    3        3

; patsy vs. nasty
(play-loop PATSY NASTY)
; score:    0      5

; nasty vs. eye-for-eye
(play-loop NASTY EYE-FOR-EYE)
; score:   1.04    .99 

; nasty vs. eye-for-eye
(play-loop NASTY EYE-FOR-EYE)
; score:   1.04    .99 

; egalitarian vs. eye-for-eye
(play-loop EGALITARIAN EYE-FOR-EYE)
; score:      3             3

; egalitarian vs. nasty
(play-loop EGALITARIAN NASTY)
; score:       .99      1.04

; eye-for-eye vs. patsy
(play-loop EYE-FOR-EYE PATSY)
; score:      3          3

; eye-for-eye vs. spastic
(play-loop EYE-FOR-EYE SPASTIC)
; score:      2.23       2.28

; results: different strategies result in significantly
; different scores. the nasty strategy tends to result
; in relatively low scores for both players, except against
; the naive patsy strategy.  the eye-for-eye strategy does
; perform quite well, seemingly because it allows cooperation
; when the other strategy permits but does not allow defection
; to go unpunished.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 3: performance of egalitarian strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the egalitarian strategy is slower to compute compared to the
; others because it is O(number of games) while the others are
; O(1).  the revised definition of egalitarian in the project 
; handout is the same order of growth as the original definition
; since it must iterate through the entire history of games to
; compute the correct play.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problems 4 and 5: eye-n-two-eyes strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (EYE-FOR-N-EYES num-eyes my-history other-history)
  (cond ((= num-eyes 0) "d")
        ((empty-history? other-history) "c")
        ((equal? (most-recent-play other-history) "c") "c")
        (else (EYE-FOR-N-EYES (-1+ num-eyes) 
                              (rest-of-plays my-history)
                              (rest-of-plays other-history)))))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (EYE-FOR-N-EYES 2 my-history other-history))

(test-equal (EYE-FOR-TWO-EYES '() '()) "c")
(test-equal (EYE-FOR-TWO-EYES '("c") '("c")) "c")
(test-equal (EYE-FOR-TWO-EYES '("c") '("c")) "c")
(test-equal (EYE-FOR-TWO-EYES '("d" "d") '("d" "d")) "d")
(test-equal (EYE-FOR-TWO-EYES '("d" "d") '("c" "d")) "c")
(test-equal (EYE-FOR-TWO-EYES '("d" "d") '("c" "c")) "c")

; egalitarian vs. eye-for-two-eyes
(play-loop EGALITARIAN EYE-FOR-TWO-EYES)
; score:      3             3

; egalitarian vs. nasty
(play-loop NASTY EYE-FOR-TWO-EYES)
; score:   1.08       .98   

; eye-for-eye vs. patsy
(play-loop EYE-FOR-EYE EYE-FOR-TWO-EYES)
; score:      3             3 

; eye-for-eye vs. spastic
(play-loop SPASTIC EYE-FOR-TWO-EYES)
; score:     3.29        1.81 

;; eye-for-two-eyes does well against strategies that do not try to
;; defect (eye-for-eye and patsy) but does poorly against strategies 
;; that frequently defect (nasty and spastic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 6: make-rotating-strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper function to determine current length of history
(define history-length length)

(define (MAKE-ROTATING-STRATEGY strat0 strat1 freq0 freq1)
  (lambda(my-history other-history)
    (let ((total-plays (history-length my-history)))
      (define i (remainder total-plays (+ freq0 freq1)))
      (if (< i freq0)
        (strat0 my-history other-history)
        (strat1 my-history other-history)))))

;; test make-rotating-strategy
(define (strat-a my-history other-history) "a")
(define (strat-b my-history other-history) "b")
(define test-rotating (MAKE-ROTATING-STRATEGY strat-a strat-b 2 1))

(test-equal (test-rotating '() '()) "a")
(test-equal (test-rotating '("a") '("a")) "a")
(test-equal (test-rotating '("a" "a") '("a" "a")) "b")
(test-equal (test-rotating '("a" "a" "b") '("a" "a" "b")) "a")

;; make-rotating-strategy performance
;; test combination of nasty and egalitarian
(define NASTY-EGALITARIAN-ROTATING (MAKE-ROTATING-STRATEGY NASTY EGALITARIAN 1 3))

(play-loop NASTY-EGALITARIAN-ROTATING EYE-FOR-TWO-EYES)
; score:      3.50                           2.24 

(play-loop NASTY-EGALITARIAN-ROTATING EGALITARIAN) 
; score:       3.49                       2.24

(play-loop NASTY-EGALITARIAN-ROTATING NASTY)
; score:       1                        1 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 7: make-higher-order-spastic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-higher-order-spastic strategies)
  (lambda(my-history other-history)
    (let ((total-plays (history-length my-history))
          (num-strategies (length strategies)))
      (define i (remainder total-plays num-strategies))
      ((list-ref strategies i) my-history other-history))))

;; test make-higher-order-spastic
(define test-higher-order-spastic
  (make-higher-order-spastic (list strat-a strat-b)))

(test-equal (test-higher-order-spastic '() '()) "a")
(test-equal (test-higher-order-spastic '("a") '("a")) "b")
(test-equal (test-higher-order-spastic '("a" "a") '("a" "a")) "a")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 8: gentle strategy generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gentle strat gentleness-factor) 
  (lambda(my-history other-history)
    (let ((orig-play (strat my-history other-history))
          (be-gentle? (<= gentleness-factor (random 1.0))))
      (if (equal? orig-play "c")
        "c"
        (if be-gentle?
          "c"
          "d")))))

;; test performance of gentle egalitarian
(define slightly-gentle-Nasty (gentle NASTY .1))
(define slightly-gentle-Eye-for-Eye (gentle EYE-FOR-EYE .1))

(play-loop slightly-gentle-Nasty slightly-gentle-Eye-for-Eye)
; score:          3.11                  2.79
; slightly-gentle-Nasty slightly beats slightly-gentle-Eye-for-Eye.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 11: three player strategy generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; problem 11 is before problems 9 and 10 because those problems 
;; use the make-combined-strategies function

(define (make-combined-strategies strat1 strat2 combine)
  (lambda(my-history other-history-1 other-history-2)
    (let ((r1 (strat1 my-history other-history-1))
          (r2 (strat2 my-history other-history-2)))
      (combine r1 r2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problems 9 and 10: 3-player prisoner dilemma
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; several functions modified in prisoner.scm to accomadate
; 3-player games

;; simple 3-player strategies
(define (Patsy-3 my-history other-history-1 other-history-2)
  (PATSY '() '()))
(define (Nasty-3 my-history other-history-1 other-history-2)
  (NASTY '() '()))
(define (Spastic-3 my-history other-history-1 other-history-2)
  (SPASTIC '() '()))

;; tough eye-for-eye strategy defects if either other player defected previously
(define tough-Eye-for-eye
  (make-combined-strategies EYE-FOR-EYE EYE-FOR-EYE
    (lambda(r1 r2) (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))))
(test-equal (tough-Eye-for-eye '() '() '()) "c")
(test-equal (tough-Eye-for-eye '("c") '("c") '("d")) "d")

;; soft eye-for-eye strategy defects if either other player defected previously
(define soft-Eye-for-eye
  (make-combined-strategies EYE-FOR-EYE EYE-FOR-EYE
    (lambda(r1 r2) (if (and (string=? r1 "d") (string=? r2 "d")) "d" "c"))))
(test-equal (soft-Eye-for-eye '() '() '()) "c")
(test-equal (soft-Eye-for-eye '("c") '("c") '("d")) "c")
(test-equal (soft-Eye-for-eye '("c") '("d") '("d")) "d")

;; play tough eye-for-eye against two soft-eye-for-eye players
(play-loop-3 tough-Eye-for-eye soft-eye-for-eye soft-eye-for-eye)
; score:         4                  4                 4

;; play tough eye-for-eye against nasty and patsy
(play-loop-3 tough-Eye-for-eye NASTY-3 PATSY-3)
; score:        2.99           3.02   .02

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 12: history summary generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-history-summary history0 history1 history2)
  (define (opp-play-history target-opp-play accum hist0 hist1 hist2)
    (define (one-play-left?) (empty-history? (rest-of-plays hist0)))
    (define (zero-plays-left?) (empty-history? hist0))
    (define (opp-plays)
      (let ((p1 (most-recent-play (rest-of-plays hist1)))
            (p2 (most-recent-play (rest-of-plays hist2))))
        (apply string-append (sort (list p1 p2) string<?))))
    (define (is-target-play?) (string=? target-opp-play (opp-plays)))
    (define (curr-play) (most-recent-play hist0))
    (cond ((zero-plays-left?) accum)
          ((one-play-left?) accum)
          (else 
            (opp-play-history target-opp-play 
                              (if (is-target-play?)
                                (if (string=? (curr-play) "c")
                                (list (1+ (car accum)) (cadr accum) (1+ (caddr accum)))
                                (list (car accum) (1+ (cadr accum)) (1+ (caddr accum))))
                                accum)
                              (rest-of-plays hist0)
                              (rest-of-plays hist1)
                              (rest-of-plays hist2)))))
  (list (opp-play-history "cc" '(0 0 0) history0 history1 history2)
        (opp-play-history "cd" '(0 0 0) history0 history1 history2)
        (opp-play-history "dd" '(0 0 0) history0 history1 history2)))

(test-equal (make-history-summary '() '() '()) '((0 0 0) (0 0 0) (0 0 0))) 
(test-equal (make-history-summary '("c") '("c") '("c")) '((0 0 0) (0 0 0) (0 0 0))) 
(test-equal (make-history-summary (list "c" "c" "d" "d" "c" "d" "c" "c")
                                  (list "c" "c" "c" "d" "d" "c" "d" "c")
                                  (list "c" "c" "d" "d" "d" "c" "c" "c"))
            '((3 0 3) (1 1 2) (0 2 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 13: probability of cooperation summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-probability-of-c hist-summary)
  (define (play-prob p)
    (if (= (caddr p) 0)
      '()
      (/ (/ (car p) (caddr p)) 1.0)))
  (map play-prob hist-summary))

(define summary (make-history-summary
                 (list "c" "c" "c" "c")
                 (list "d" "d" "d" "c")
                 (list "d" "d" "c" "c")))
(test-equal (get-probability-of-c summary) '(1. 1. 1.))
(define new-summary (make-history-summary
                     (list "c" "c" "c" "d" "c")
                     (list "d" "c" "d" "d" "c")
                     (list "d" "c" "c" "c" "c")))
(test-equal (get-probability-of-c new-summary) (list .5 1. (quote ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; problem 14: deciphering other player's strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; in expected-values: #f = don't care 
;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

;; detect soft-eye-for-eye strategy
(define (detect-soft-Eye-for-eye hist0 hist1 hist2)
  (test-entry '(1 1 0)
              (get-probability-of-c (make-history-summary hist0 hist1 hist2))))

; test positive detection
(test-equal (detect-soft-Eye-for-eye
                     (list "d" "c" "c" "c" "c")
                     (list "d" "d" "d" "d" "c")
                     (list "d" "d" "c" "c" "c")) true)
; test negative detection
(test-equal (detect-soft-Eye-for-eye
                     (list "c" "c" "c" "c" "c")
                     (list "d" "d" "d" "d" "c")
                     (list "d" "d" "c" "c" "c")) false)

;; new strategy that detects patsies
(define (dont-tolerate-fools my-history other-history-1 other-history-2)
  (if (<= (length my-history) 10)
    "c"
    (if (and (could-he-be-a-fool? other-history-1 my-history other-history-2)
             (could-he-be-a-fool? other-history-2 my-history other-history-1))
      "d"
      "c")))

;; test new patsy detection strategy
(play-loop-3 dont-tolerate-fools PATSY-3 PATSY-3)
; score:          4.88             2.24   2.24

(play-loop-3 dont-tolerate-fools tough-Eye-for-eye soft-Eye-for-eye)
; score:            3.94                3.97             3.91

;; indeed the strategy detects when the other opponents are fools and does
;; exploit them
