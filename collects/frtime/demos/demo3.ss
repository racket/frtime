(module demo3 (lib "animation.ss" "frtime")
  
  (require (lib "math.ss"))
  
  (let* ([radius 100]
         [extra-count (keyboard-control 'up 'down 3)]
         [cvt (lambda (z) (+ 200 (* radius z)))]
         [x (cos (/ time-b 300))]
         [y (sin (/ time-b 300))]
         [green (+ .75 (* .25 x))]
         [blue (+ .75 (* .25 y))])
    (display-shapes
     (drop (- 3 extra-count)
           (list
            (make-ring (make-posn (cvt (/ x 4)) (cvt (/ y 4))) (* radius .75) "gray")
            (make-circle (make-posn (cvt (- (/ x 2))) (cvt (- (/ y 2)))) 8 "gray")
            (make-ring (make-posn (cvt (- (/ x 2))) (cvt (- (/ y 2)))) (/ radius 2) "gray")
            (make-ring (make-posn 200 200) radius "gray")
            (make-line (make-posn (cvt -1.2) 200) (make-posn (cvt 1.2) 200) "gray")
            (make-line (make-posn 200 (cvt -1.2)) (make-posn 200 (cvt 1.2)) "gray")
            (make-circle (make-posn (cvt x) (cvt y)) 10 (make-rgb 0 green blue))
            (make-circle (make-posn 200 (cvt (- y))) 10 (make-rgb 0 0.75 (- 1.5 blue)))
            (make-circle (make-posn (cvt (- x)) 200) 10 (make-rgb 0 (- 1.5 green) 0.75)))))))
