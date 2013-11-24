;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (+ (* .5 (* a (square t)) )
          (* v t))
       u)))

;; you need to complete this procedure, then show some test cases

(position 0 0 0 0)    ; -> 0
(position 0 0 20 0)   ; -> 20
(position 0 5 10 10)  ; -> 60
(position 2 2 2 2)    ; -> 10
(position 5 5 5 5)    ; -> 92.5
(position 0 0 0 100)  ; -> 0
(position 4 4 4 0)    ; -> 4


;; Problem 2

(define quadratic-formula 
  (lambda (a b c sign)
    (let ((quad-root (- (square b)
                        (* 4 (* a c)))))
    (if (< quad-root 0)
      false
      (/ (sign (- b) (sqrt quad-root))
         (* 2 a))))))

(define root1
  (lambda(a b c) (quadratic-formula a b c +)))

(define root2
  (lambda(a b c) (quadratic-formula a b c -)))

(root1 5 3 6)       ; -> false
(root2 5 3 6)       ; -> false
(root1 1 3 -4)      ; -> 1
(root2 1 3 -4)      ; -> -4

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (* -.5 gravity) vertical-velocity elevation)))

(time-to-impact 10 5)  ; -> -.4154...

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

(time-to-height 10 5 0) ; -> 2.4562...
(time-to-height 10 5 1) ; -> 2.3833...

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-time-simple
  (lambda (elevation velocity angle)
    (let ((y-vel (* (sin (degree2radian angle))
                    velocity)))
      (time-to-impact y-vel elevation))))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((x-vel (* (cos (degree2radian angle))
                    velocity)))
      (position 0 x-vel 0 (travel-time-simple elevation velocity angle)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; what is the distance traveled in each case?
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
(travel-time-simple 1 45 0)     ; -> 0.4518 sec
(travel-distance-simple 1 45 0) ; -> 20.33 m

;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
(travel-time-simple 1 45 90)     ; -> 9.21 sec
(travel-distance-simple 1 45 90)     ; -> 0 m 

;; at an angle of (/ pi 4) radians or 45 degrees
(travel-time-simple 1 45 45)     ; -> 6.53 sec
(travel-distance-simple 1 45 45)     ; -> 207.63 m 

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define rec-find-best-angle
  (lambda (velocity elevation current-max-angle current-max-distance current-angle)
    (let ((current-distance (travel-distance-simple elevation velocity current-angle)))
      (if (> current-angle 90)
        current-max-angle
        (if (> current-distance current-max-distance)
          (rec-find-best-angle velocity elevation current-angle current-distance (+ current-angle alpha-increment))
          (rec-find-best-angle velocity elevation current-max-angle current-max-distance (+ current-angle alpha-increment)))))))

(define find-best-angle
  (lambda (velocity elevation)
    (rec-find-best-angle velocity elevation 0 0 0)))

;; find best angle for velocity of 45 m/s and height of 1 m
(find-best-angle 45 1) ; -> 44.86 degrees

;; try for other velocities
(find-best-angle 90 1) ; -> 44.97 degrees
(find-best-angle 5 1)  ; -> 36.82 degrees

;; try for other heights
(find-best-angle 45 100) ; -> 35.48 degrees
(find-best-angle 45 0)   ; -> 45.00 degrees

;; result: both the velocity and height affect the optimum angle

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 t0 dt g m beta ret)
    (let ((dx (* u0 dt))
          (dy (* v0 dt))
          (du (* (* (* (/ -1 m) beta)
                    (sqrt (+ (square u0) (square v0))))
                 (* u0 dt)))
          (dv (* (+ g (* (* (/ 1 m) (* beta v0))
                          (sqrt (+ (square u0) (square v0)))))
                 (* -1 dt))))
      (if (< y0 0)
        (ret x0 y0 u0 v0 t0) 
        (integrate (+ x0 dx)
                   (+ y0 dy)
                   (+ u0 du)
                   (+ v0 dv)
                   (+ t0 dt)
                   dt g m beta ret)))))

(define travel-distance
  (lambda(elevation velocity angle beta)
    (travel-fun elevation velocity angle beta 
                     (lambda(x0 y0 u0 v0 t0) x0))))

(define travel-fun
  (lambda (elevation velocity angle beta ret)
    (let ((u0 (* velocity 
                 (cos (degree2radian angle))))
          (v0 (* velocity
                 (sin (degree2radian angle)))))
      (integrate 0 elevation u0 v0 0 .01 gravity mass beta ret))))

;; test for an angle of 45 degrees and initial velocities of 45, 40, and 35 m/sec
(travel-distance 1 45 45 beta) ; -> 92.23 m
(travel-distance 1 40 45 beta) ; -> 81.66 m
(travel-distance 1 35 45 beta) ; -> 70.30 m
;; all of the above distances seem reasonable given that they are
;; approximately the size of a baseball field.

;; what about Denver?
(define denver-density 1.06)  ; kg/m^3
(define denver-beta (* .5 drag-coeff denver-density (* 3.14159 .25 (square diameter))))
(travel-distance 1 45 45 denver-beta) ; -> 99.83 m
(travel-distance 1 40 45 denver-beta) ; -> 87.72 m
(travel-distance 1 35 45 denver-beta) ; -> 74.95 m
;; as expected the ball flies slightly farther in the thinner air in denver
;; in these examples the effect is around +5-10%

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define travel-time
  (lambda(elevation velocity angle beta)
    (travel-fun elevation velocity angle beta 
                     (lambda(x0 y0 u0 v0 t0) t0))))

(define rec-shortest-throwing-time
  (lambda (velocity target height best-time current-angle)
    (let ((angle-step .25)
          (angle-max 90)
          (close-enough? (lambda(distance target)
                         (< (abs (- distance target)) 1)))
          (current-distance (travel-distance height velocity current-angle beta)))
      (define new-best-time (if (close-enough? current-distance target)
                              (if (= best-time 0)
                                (travel-time height velocity current-angle beta)
                                (min (travel-time height velocity current-angle beta)
                                     best-time))
                              best-time))

      (if (> current-angle angle-max)
        best-time
        (rec-shortest-throwing-time velocity 
                                    target 
                                    height 
                                    new-best-time 
                                    (+ current-angle angle-step))))))

(define shortest-throwing-time 
  (lambda (velocity target height)
    (rec-shortest-throwing-time velocity target height 0 -90)))

;; a catcher trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?
;(shortest-throwing-time 55 36 1) ; -> .77 s
;(shortest-throwing-time 45 36 1) ; -> .93 s
;(shortest-throwing-time 35 36 1) ; -> 1.2 s
;; these seem like reasonable times

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s
;(shortest-throwing-time 45 90 1) ; -> 3.59 s

;; Problem 8
(define travel-distance-with-bounces
  (lambda(height velocity angle num-bounces)
    (if (< num-bounces 0)
      0
      (+ (travel-distance height velocity angle beta)
         (travel-distance-with-bounces   0
                                         (/ velocity 2)
                                         angle
                                         (-1+ num-bounces))))))

;; should be the same as travel-distance
(travel-distance-with-bounces 1 45 45 0) ; -> is indeed the same, 92.23 m
;; should get progressively longer with more bounces
(travel-distance-with-bounces 1 45 45 1) ; -> 130.61 m
(travel-distance-with-bounces 1 45 45 2) ; -> 142.54 m
(travel-distance-with-bounces 1 45 45 10) ; -> 146.89 m
;; distance does indeed increase as the number of bounces increases

;; different initial velocities and angles
(travel-distance-with-bounces 1 20 45 10) ; -> 45.84 m 
(travel-distance-with-bounces 1 45 25 10) ; -> 130.87 m 
(travel-distance-with-bounces 1 15 65 10) ; -> 21.24 m

;; Problem 9
