(module demo3 (lib "frtime.ss" "frtime")
  
  (require
   (lib "animation.ss" "frtime")
   (lib "gui.scm" "frtime"))
  
  (let* ([radius (make-slider "Radius" 50 150 100)]
         [speed (* .01 (make-slider "Speed" 25 75 50))]
         [phase (wave speed)]
         [ring1 (make-check-box "Show x-y ring")]
         [center (make-check-box "Show center of x-y ring")]
         [ring2 (make-check-box "Show pt-center ring")]
         [cvt (lambda (z) (+ 200 (* radius z)))]
         [x (cos phase)]
         [y (sin phase)]
         [green (+ .75 (* .25 x))]
         [blue (+ .75 (* .25 y))])
    (display-shapes
     (list
      (when ring2 (make-ring (make-posn (cvt (/ x 4)) (cvt (/ y 4))) (* radius .75) "gray"))
      (when center (make-circle (make-posn (cvt (- (/ x 2))) (cvt (- (/ y 2)))) (/ radius 12) "gray"))
      (when ring1 (make-ring (make-posn (cvt (- (/ x 2))) (cvt (- (/ y 2)))) (/ radius 2) "gray"))
      (make-ring (make-posn 200 200) radius "gray")
      (make-line (make-posn (cvt -1.2) 200) (make-posn (cvt 1.2) 200) "gray")
      (make-line (make-posn 200 (cvt -1.2)) (make-posn 200 (cvt 1.2)) "gray")
      (make-circle (make-posn (cvt x) (cvt y)) (/ radius 10) (make-rgb 0 green blue))
      (make-circle (make-posn 200 (cvt (- y))) (/ radius 10) (make-rgb 0 0.75 (- 1.5 blue)))
      (make-circle (make-posn (cvt (- x)) 200) (/ radius 10) (make-rgb 0 (- 1.5 green) 0.75))))))
