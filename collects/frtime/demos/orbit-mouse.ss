(module demo1 (lib "frtime.ss" "frtime")
  
  (require (lib "animation.ss" "frtime")
           (as-is/unchecked (lib "math.ss") pi)
           (lib "etc.ss" "frtime"))
  
  (define radius 25)
  (define speed (+ .4 (* .2 (range-control (key 'right) (key 'left) 8))))
  (define phase (wave speed))
  (define n (add1 (range-control (key 'up) (key 'down) 5)))
  
  (display-shapes
   (build-list
    n
    (lambda (i)
      (let ([t (+ (/ (* 2 pi i) n) phase)])
        (make-circle
         (posn+ mouse-pos
                (make-posn
                 (* radius (cos t))
                 (* radius (sin t))))
         5 "blue")))))
  
  (provide (all-defined-except)))
