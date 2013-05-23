(define (my-car x)
  (if (pair? x)
    (car x)
    '()))

(define (my-cdr x)
  (if (pair? x)
    (cdr x)
    '()))

(define (my-char=? x y)
  (if (char? x)
    (char=? x y)
    #f))

(define (make-vector-and-init size val)
  (let
    ((vec (make-vector size)))
    (let loop ((x 0))
      (if (< x size)
        (begin
          (vector-set! vec x val)
          (loop (+ x 1)))))
    vec))

(define (vector-init vec val)
  (do ((i 0 (+ i 1))) ((= i (vector-length vec)))
    (vector-set! vec i val)))
