(define (make-enumeration enumeration-list)
  (lambda (selector)
    (case selector
      ((get-last)
       (lambda () (- (length enumeration-list) 1)))
      ((display)
       (lambda ()'()))
      (else
       (error "make-enumeration" "Invalid selector: " selector)))))
