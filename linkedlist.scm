
(define (make-linked-list)
  (let
    ((linked-list '())
     (tail '()))

    (letrec

      ((add-to-list
        (lambda (x)
          (if (null? linked-list)
            (begin
              (set! linked-list (list x))
              (set! tail linked-list))
            (begin
              (set-cdr! tail (list x))
              (set! tail (cdr tail))))))

      (append-to-list
        (lambda (x)
          (set! x (clone-spine x))
          (if (null? linked-list)
            (set! linked-list x)
            (if (not (null? tail))
              (set-cdr! tail x)))
          (if (not (null? x))
            (set! tail (last x)))))

      (initialize-list
        (lambda ()
          (set! linked-list '())
          (set! tail '())))

      (get-list
        (lambda () linked-list))

      (last
        (lambda (x)
          (cond
            ((null? x) x)
            ((null? (cdr x)) x)
            (else
              (last (cdr x))))))

      (clone-spine
        (lambda (x)
          (cond
            ((null? x) x)
            (else
              (cons (my-car x) (clone-spine (my-cdr x)))))))

      (dispatch
        (lambda (message)
          (cond
            ((eq? message 'add) add-to-list)
            ((eq? message 'append) append-to-list)
            ((eq? message 'initialize) (initialize-list))
            ((eq? message 'newline) (lambda () "")) ; do nothing
            ((eq? message 'get-linked-list) get-list)
            (else
              (begin
                (display "unknown request ")
                (display message) (newline)))))))
    dispatch)))
