(module demo2 (lib "animation.ss" "frtime")
  
  (require (lib "math.ss")
           (lib "gui.scm" "frtime"))
  
  (display-shapes
   (let* ([x-center 300]
          [y-center 200]
          ;[radius (+ 30 (range-control (key #\W) (key #\w) 20))]
          [radius (make-slider "Radius" 30 50 30)]
          [cylinder-width (* 2 radius)]
          [hatch-frac 0.6]
          ;[crank (+ 150 (range-control (key #\C) (key #\c) 50))]
          [crank (make-slider "Crank length" 150 200 150)]
          [piston-height (+ 20 (range-control (key #\P) (key #\p) 20))]
          [piston-width 12]
          [gap (/ cylinder-width 6)]
          ;[speed (+ .5 (* .2 (range-control (key 'right) (key 'left) 10)))]
          [speed (* .02 (make-slider "Speed" 25 75 25))]
          [phase (wave speed)]
          [x1 (* radius (cos phase))]
          [y1 (* radius (sin phase))]
          [x0 (+ x1 (sqrt (- (sqr crank) (sqr y1))))])
     (list
      ; wheel
      (make-ring (make-posn x-center y-center) radius "black")
      ; crank
      (make-line (make-posn (- x-center x1) (+ y-center y1))
                 (make-posn (- x-center x0) y-center)
                 "black")
      ; gas in cylinder
      (make-rect (make-posn (- x-center crank radius gap piston-width)
                            (- y-center (/ piston-height 2)))
                 (- (+ radius gap crank 6) x0)
                 piston-height
                 (let ([c (/ x1 30)])
                   (fix-rgb 1
                            (- .8 (* .3 c))
                            (- .8 (* .3 c)))))
      ; piston head
      (make-rect (make-posn (- x-center x0 piston-width)
                            (- y-center (/ piston-height 2)))
                 piston-width piston-height "black")
      ; blue ball
      (make-circle (make-posn (- x-center x1) (+ y-center y1)) 5 "blue")
      ; cross on wheel
      (make-line (make-posn (- x-center (* hatch-frac x1)) (+ y-center (* hatch-frac y1)))
                 (make-posn (+ x-center (* hatch-frac x1)) (- y-center (* hatch-frac y1))) "black")
      (make-line (make-posn (- x-center (* hatch-frac y1)) (- y-center (* hatch-frac x1)))
                 (make-posn (+ x-center (* hatch-frac y1)) (+ y-center (* hatch-frac x1))) "black")
      ; cylinder outline
      (make-line (make-posn (- x-center radius crank piston-width gap)
                            (- y-center (/ piston-height 2) 1))
                 (make-posn (+ (- x-center radius crank piston-width) cylinder-width)
                            (- y-center (/ piston-height 2) 1)) "black")
      (make-line (make-posn (- x-center radius crank piston-width gap)
                            (+ y-center (/ piston-height 2)))
                 (make-posn (+ (- x-center radius crank piston-width) cylinder-width)
                            (+ y-center (/ piston-height 2))) "black")
      (make-line (make-posn (- x-center radius crank piston-width gap 1)
                            (- y-center (/ piston-height 2) 1))
                 (make-posn (- x-center radius crank piston-width gap 1)
                            (+ y-center (/ piston-height 2))) "black")))))
