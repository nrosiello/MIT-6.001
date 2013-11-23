; Project 0: Examples of simple Scheme expressions
+

(- (if (> 3 4)
     7
     10)
   (/ 16 10))

(define double (lambda (x) (* 2 x)))

double

(define c 4)

c

(double c)

c

(double (double (+ c 5)))

(define times-2 double)

(times-2 4)
