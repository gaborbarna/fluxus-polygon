(require racket/math)

(define (get-phi)
  (let ([p (- pi)])
    (lambda (n)
      (set! p (+ p (/ (* 2 pi) n)))
      p)))

(define (in-range x a b)
  (and (> x a) (<= x b)))

(define (make-poly n)
  (with-state
   (hint-unlit)
   (hint-wire)
   (backfacecull 0)
   (wire-colour (vector 1 0 0))
   (colour (vector 1 1 1))
   (build-polygons n 'polygon)))

(define poly (make-poly 64))

(with-primitive poly
		(pdata-add "circle" "v")
		(let ([next-phi (get-phi)])
		  (pdata-map! 
		   (lambda (circle) 
		     (let ([phi (next-phi (pdata-size))])
		       (vector (cos phi) (sin phi) 0))) 
		   "circle"))

		(pdata-add "square" "v")
		(let ([next-phi (get-phi)]
		      [max (/ (sqrt 2) 2)])
		  (pdata-map!
		   (lambda (square)
		     (let ([phi (next-phi (pdata-size))])
		       (cond ((in-range phi (/ (- pi) 4) (/ pi 4)) ;(-pi/4, pi/4)
			      (vector max (sin phi) 0))
			     ((in-range phi (/ pi 4) (* 3 (/ pi 4))) ;(pi/4, 3pi/4)
			      (vector (cos phi) max 0))
			     ((in-range phi (* 3 (/ (- pi) 4)) (* 3 (/ pi 4))) ;(-3pi/4, 3pi/4)
			      (vector (cos phi) (- max) 0))
			     (else ;(-3pi/4 -pi/4)
			      (vector (- max) (sin phi) 0)))))
		   "square"))

		(pdata-map!
		 (lambda (p square) square)
		 "p" "square"))

(define (transform-polygon)
  (define (transform str)
    (with-primitive poly
		    (pdata-map!
		     (lambda (p o)
		       (vadd p (vdiv (vsub o p) 20)))
		     "p" str)))
  (lambda (o)
    (cond ((eq? o 'to-circle)
	   (transform "circle"))
	  ((eq? o 'to-square)
	   (transform "square")))))

(define old-time (time))		   

(every-frame
 (let ([t (- (time) old-time)])
   (begin
     (when (in-range t 5 15)
       ((transform-polygon) 'to-circle))
     (when (in-range t 15 25)
       ((transform-polygon) 'to-square)))))

