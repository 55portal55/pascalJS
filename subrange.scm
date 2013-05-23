(define (error x y z) ; stub out
  '())

(define (make-subrange first last)
  (lambda (selector)
    (case selector
      ((get-first)
       (lambda () first))
      ((get-last)
       (lambda () last))
      ((display)
       (lambda ()
         (display "first: ") (display first) (newline)
         (display "last: ") (display last)))
      (else
       (error "make-subrange" "Invalid selector: " selector)))))
