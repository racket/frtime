(lambda (sym fail)
  (case sym
    [(name) "Make"]
    [(help-desk-message) (format "Mz/Mr: ~s" `(require-library "make.ss" "make"))]
    [(blurb)
     (list
      "The make collection provides a facility similary to unix's \"make\" command")]
    [else (fail)]))
