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
    (+ (* (/ a 2)
	  (square t))
       (* v t)
       u)))

;; you need to complete this procedure, then show some test cases

; (position 0 0 0 0)
; (position 0 0 20 0)
; (position 0 5 10 10)
; (position 2 2 2 2)
; (position 5 5 5 5)


;; Problem 2

(define root1
  (lambda (a b c)
    (let ((D (- (square b)              ;b^2 - 4ac
		(* 4 a c))))
      (if (< D 0)
	  false				;invalid input
	  (/ (- (- b) (sqrt D))
	     (* 2 a))))))

(define root2
  (lambda (a b c)
    (let ((D (- (square b)              ;b^2 - 4ac
		(* 4 a c))))
      (if (< D 0)
	  false				;invalid input
	  (/ (+ (- b) (sqrt D))
	     (* 2 a))))))
;; complete these procedures and show some test cases
(root1 5 3 6)				;when D < 0
(root1 1 2 1)				;when D = 0
(root1 1 4 2)				;when D > 0

(root2 5 3 6)				;when D < 0
(root2 1 2 1)				;when D = 0
(root2 1 4 2)				;when D > 0

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (if (and (negative? elevation)
             (negative? vertical-velocity)) ;cannot hit the grount at any time > 0
        false
        (root1 (- (/ gravity 2)) vertical-velocity elevation)))) ;with other possibitities coped by root1 procedure.
    ;; (let ((t1 (root1 (- (/ gravity 2)) vertical-velocity elevation))
    ;; 	  (t2 (root2 (- (/ gravity 2)) vertical-velocity elevation)))
    ;;   (cond ((> t1 0)
    ;; 	     (display "first path")
    ;; 	     t1)
    ;; 	    ((> t2 0)
    ;; 	     (display "second path")
    ;; 	     t2)
    ;; 	    (else false)))))		;error we assume the flight time should positive number.


;; (time-to-impact 10 3)			;positive initial velocity with positive elevation
;; (time-to-impact -20 3)			;negative initial velocity with positive elevation
;; (time-to-impact 1 -2)			;positive initial velocity with negative elevation, whose condition cannot hit the ground. Note that it handled by root1 procedure.
;; (time-to-impact -2 -3)			;negative initial velocity with negative elevation; cannot hit the ground.

;; Note that if we want to know when the ball drops to a particular height r
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (let ((v_x (* velocity
                    (cos alpha)))
            (v_y (* velocity
                    (sin alpha))))
        (* v_x
           (time-to-impact v_y elevation))))))

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
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; (define case1 (travel-distance-simple 1 45 0)) ;case 1
;; case1                                          ;meter
;; (meters-to-feet case1)                         ;feet
;; (define case2 (travel-distance-simple 1 45 45)) ;case2
;; case2                                           ;meter
;; (meters-to-feet case2)                          ;feet
;; (define case3 (travel-distance-simple 1 45 90)) ;case 3
;; case3                                           ;meter
;; (meters-to-feet case3)                          ;feet

;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define alpha-increment 0.01)

(define (iterate-on-angle lower upper method update? return)
  (lambda (velocity elevation)
    (define increment 0.1)
    (define (next ang) (+ ang increment))
    (define (iter angle extremum ext-ang)
      (if (>= angle upper)
          (return ext-ang extremum)
          (let ((result (method elevation velocity angle)))
            (let ((next-ang (next angle)))
              (if (update? result extremum)
                  (iter next-ang result angle)
                  (iter next-ang extremum ext-ang))))))
    (iter lower 0 0)))

(define (iterate-on-angle-with-dist method)
  (iterate-on-angle 0 90 method
                    (lambda (current max) (> current max))
                    (lambda (ang dist) ang)))

(define find-best-angle
  (iterate-on-angle-with-dist travel-distance-simple))

(define test-effect-of-angle-and-best-angle
  (iterate-on-angle-with-dist
   (lambda (elevation velocity angle)
     (let ((distance (travel-distance elevation velocity angle)))
       (if (homerun? distance)          ;display result
           (begin (newline)
                  (display "Angle (degrees) : ")
                  (display angle)
                  (display "\tdistance (meters) : ")
                  (display distance)
                  (display "\t(feets) : ")
                  (display (meters-to-feet distance))))
       distance))))

(define (homerun? distance)
  (> (meters-to-feet distance) 300))

;; find best angle
;; try for other velocities
;; try for other heights

;; (find-best-angle 45 1)			;returns 45
;; (find-best-angle 23 0.8)		;returns 45
;; (find-best-angle 21 10)			;returns 40
;; ↑ is not practical to our situation where we consider the baseball hitten by batter.
;; We should restrict the elevation at most 1 meter from the ground.

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
;; (define density 1.25)  ; kg/m^3
(define density 1.06)  ; for denver
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define (integrate-gen terminate? select)
  (lambda (x0 y0 u0 v0 dt g m beta)
    (define (iter x y u v t)
      (if (terminate? x y u v t)        ;termination condition
          (select x y u v t)
          (let ((speed (sqrt (+ (square u)
                                (square v))))
                (v-factor (* (/ 1 m)
                             beta)))
            (let ((dx (* u dt))
                  (dy (* v dt))
                  (du (* (- v-factor)
                         speed
                         u
                         dt))
                  (dv (* (- (+ (* v-factor
                                  speed
                                  v)
                               g))
                         dt)))
              (iter (+ x dx)            ;transition
                    (+ y dy)
                    (+ u du)
                    (+ v dv)
                    (+ t dt))))))
    (iter x0 y0 u0 v0 0)))

(define integrate                       ;returns x when y becomes negative.
  (integrate-gen
   (lambda (x y u v t) (< y 0))
   (lambda (x y u v t) x)))

(define (travel method elevation speed angle)
  (let ((alpha (degree2radian angle)))
    (method 0
            elevation
            (* speed
               (cos alpha))
            (* speed
               (sin alpha))
            0.01
            gravity
            mass
            beta)))

(define (travel-distance elevation speed angle)
  (travel integrate elevation speed angle))



;; RUN SOME TEST CASES
;; (meters-to-feet (travel-distance 1 45 45)) ; 304.4 Home run
;; (meters-to-feet (travel-distance 1 40 45)) ; 269.5 Oh... sorry about that
;; (meters-to-feet (travel-distance 1 35 45)) ; 232.0 Fly out
;; (find-best-angle 45 1 travel-distance)	   ; Home run range (32 48)
;; (test-effect-of-angle-and-best-angle 45 1)

;; what about Denver?

;; (meters-to-feet (travel-distance 1 45 45)) ;329.4 Home run
;; (meters-to-feet (travel-distance 1 40 45)) ;289.4 Sorry for that
;; (meters-to-feet (travel-distance 1 35 45)) ;247.3 Fly out
;; (find-best-angle 45 1 travel-distance)	   ;Home run range (26 55)
;; Problem 7

;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to
;; use, given a velocity, in order to reach a given height (receiver) at a
;; given distance

;; numerical calculator
(define (travel-distance-with-time elevation speed angle)
  (travel
   (integrate-gen
    (lambda (x y u v t) (< y 0))
    (lambda (x y u v t) (make-dist-time x t)))
   elevation speed angle))

;; wrapper structure
(define (make-dist-time x t) (cons x t))
(define (dist p) (car p))
(define (time p) (cdr p))

;; governing the iteration
(define (throw-desired-distance velocity desired-distance height)
  (let ((epsilon 0.5))                  ;distance tolerance (m)
    ((iterate-on-angle
      -90                               ;lower bound angle
      90                                ;upper bound angle
      (lambda (elevation velocity angle)
        (let ((result (travel-distance-with-time elevation velocity angle))) ;distance-time pair
          (if (< (abs (- (dist result) desired-distance)) epsilon) ;within tolerance?
              (time result)             ;return that time
              0)))                      ;return default time
      (lambda (current min)             ;update condition
        (and (not (zero? current))
             (or (zero? min)
                 (< current min))))
      cons)         ;construct pair that contains angle with minimum travel time
     velocity height)))     ;initial velocity and height at which the throw made

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft)
;; using 45m/s

;; Problem 8
(define (travel-distance-with-bounces elevation speed angle bounces)
  (define (iter vel remaining-bounces sum-dist)
    (if (zero? remaining-bounces)
        sum-dist
        (iter (/ vel 2.) (-1+ remaining-bounces)
              (+ sum-dist (travel-distance 0 vel angle)))))
  (iter (/ speed 2.) bounces (travel-distance elevation speed angle))) ;initial condition
;; Problem 9
