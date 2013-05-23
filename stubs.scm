; if we are just interested in converting pascal to scheme
; then stub out the scheme evaluation routines.

(define (my-display x)
  (display x))

(define (my-write x)
  (write x))

(define (my-newline)
  (newline))

(define (flush-output)
  (newline))

(define (scheme-eval scheme-code)
  scheme-code)
