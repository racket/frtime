(module pong (lib "frtime.ss" "frtime")
  
  (require
   (lib "animation.ss" "frtime")
   (all-except (lib "match.ss") match))
  
  (define paddle-radius 20)
  (define (neg-x p)
    (make-posn (- (posn-x p)) (posn-y p)))
  (define (neg-y p)
    (make-posn (posn-x p) (- (posn-y p))))
  
  (define pos1
    (let ([paddle2-pos (make-posn (clip (posn-x mouse-pos) 230 370) (clip (posn-y mouse-pos) 30 370))]
          [collide (match-lambda
                     [(_ mp p)
                      (let ([u (normalize (posn- mp p))])
                        (lambda (v)
                          (posn- v (posn* u (* 2 (posn-dot v u))))))])])
      (letrec ([paddle1-pos (make-posn
                             (clip (+ 100
                                      (integral (hold
                                                 (merge-e
                                                  ((snapshot-e key-strokes (posn-x paddle1-pos)) . =#=> .
                                                   (match-lambda
                                                     [('release _) 0]
                                                     [((or 'numpad1 'numpad4 'numpad7) p) (if (> p 30)
                                                                                              -0.25
                                                                                              nothing)]
                                                     [((or 'numpad3 'numpad6 'numpad9) p) (if (< p 170)
                                                                                              .25
                                                                                              nothing)]
                                                     [_ nothing]))
                                                  ((when-e (>= (posn-x paddle1-pos) 170)) . -=> . 0)
                                                  ((when-e (<= (posn-x paddle1-pos) 30)) . -=> . 0))
                                                 0)))
                                   30 170)
                             (clip (+ 100
                                      (integral (hold
                                                 (merge-e
                                                  ((snapshot-e key-strokes (posn-y paddle1-pos)) . =#=> .
                                                   (match-lambda
                                                     [('release _) 0]
                                                     [((or 'numpad7 'numpad8 'numpad9) p) (if (> p 50)
                                                                                              -0.25
                                                                                              nothing)]
                                                     [((or 'numpad1 'numpad2 'numpad3) p) (if (< p 350)
                                                                                              .25
                                                                                              nothing)]
                                                     [_ nothing]))
                                                  ((when-e (>= (posn-y paddle1-pos) 370)) . -=> . 0)
                                                  ((when-e (<= (posn-y paddle1-pos) 30)) . -=> . 0))
                                                 0)))
                                   30 370))]
               [pos1 (switch
                      ((merge-e
                        (when-e (> (posn-x pos1) 500))
                        (when-e (< (posn-x pos1) -100))
                        (when-e (> (posn-y pos1) 500))
                        (when-e (< (posn-y pos1) -100))) . -=> . (posn+ (make-posn 100 100) (posn-integral vel1)))
                                            (posn+ (make-posn 100 100) (posn-integral vel1)))]
               [vel1 (accum-b
                      (merge-e
                       ((merge-e
                         (when-e (> (posn-x pos1) 390))
                         (when-e (< (posn-x pos1) 10))) . -=> . neg-x)
                       ((merge-e
                         (when-e (> (posn-y pos1) 390))
                         (when-e (< (posn-y pos1) 10))) . -=> . neg-y)
                       ((merge-e
                         (snapshot-e (when-e (< (posn-diff pos1 paddle1-pos)
                                                (+ 10 paddle-radius))) paddle1-pos pos1)
                         (snapshot-e (when-e (< (posn-diff pos1 paddle2-pos)
                                                (+ 10 paddle-radius))) paddle2-pos pos1))
                        . ==> . collide))
                      (make-posn .29 .23))])
        (let ([p1-score (accum-b
                         (merge-e
                          ((key #\r) . -=> . (lambda (x) 0))
                          ((snapshot-e (when-e (< (posn-x pos1) 10)) (posn-y pos1))
                           . =#=> .
                           (match-lambda
                             [(_ y) (if (and (> y 150) (< y 250))
                                        add1
                                        nothing)])))
                         0)]
              [p2-score (accum-b
                         (merge-e
                          ((key #\r) . -=> . (lambda (x) 0))
                          ((snapshot-e (when-e (> (posn-x pos1) 390)) (posn-y pos1))
                           . =#=> .
                           (match-lambda
                             [(_ y) (if (and (> y 150) (< y 250))
                                        add1
                                        nothing)])))
                         0)])
          (display-shapes
           (list 
            (make-line (make-posn 200 0) (make-posn 200 399) "gray")
            (make-circle pos1 10 "blue")
            (make-circle paddle1-pos paddle-radius "black")
            (make-circle paddle2-pos paddle-radius "black")
            (make-graph-string (make-posn 30 30) (number->string p2-score) "black")
            (make-graph-string (make-posn 350 30) (number->string p1-score) "black")
            (make-graph-string (make-posn 120 30) (number->string (posn-len vel1)) "black")
            (make-line (make-posn 0 150) (make-posn 0 250) "red")
            (make-line (make-posn 399 150) (make-posn 399 250) "red")))
          pos1)))))
