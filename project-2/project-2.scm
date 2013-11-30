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

