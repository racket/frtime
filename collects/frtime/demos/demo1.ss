(module demo1 (lib "animation.ss" "frtime")
  
  (require (lib "math.ss"))
  
  (display-shapes
   (let* ([radius 25]
          [speed (+ .4 (* .2 (range-control (key 'right) (key 'left) 8)))]
          [phase (wave speed)]
          [n (add1 (range-control (key 'up) (key 'down) 5))]) ; use up and down arrow
     (build-list
      n
      (lambda (i)
        (let ([t (+ (/ (* 2 pi i) n) phase)])
          (make-circle
           (posn+ mouse-pos
                  (make-posn
                   (* radius (cos t))
                   (* radius (sin t))))
           5 "blue")))))))
  