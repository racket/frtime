(module demo5 (lib "animation.ss" "frtime")
  
  (display-shapes
   (letrec ([pos (posn-integral vel)]
            [vel (posn/ (posn- mouse-pos pos) 400.0)])
     (list
      (make-line mouse-pos pos "gray")
      (make-circle pos (+ 10 (/ 400 (+ 40.0 (posn-diff mouse-pos pos)))) "blue")))))
