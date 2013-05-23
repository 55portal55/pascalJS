
;;; define hash table routines

(define (find-binding name a-list)
  (let
    ((binding (assq name a-list)))
    (if (null? binding)
      '()
      (if binding
        binding
        '()))))

(define (find-value name a-list)
  (let
    ((binding (find-binding name a-list)))
    (if (null? binding)
      '()
      (cadr binding))))

(define (get-hash-index key table-size)
  (if (not (symbol? key))
    0
    (let*
      ((key-string (symbol->string key))
       (key-length (string-length key-string))
       (hash-index
         (lambda (n)
           (remainder n table-size))))
      (cond
        ((zero? key-length) 0)
        ((= key-length 1)
          (hash-index (char->integer (string-ref key-string 0))))
        (else
          (hash-index
            (*
              (char->integer (string-ref key-string 0))
              (char->integer (string-ref key-string 1)))))))))

(define (hash-binding table key index)
  (find-binding key (vector-ref table index)))

(define (hash-ref table key)
  (let
    ((binding
      (find-binding
        key
        (vector-ref
          table
          (get-hash-index key
            (vector-length table))))))
    (if (not (null? binding))
      (cadr binding)
      '())))

(define (hash-set! table key value)
  (let*
    ((hash-index (get-hash-index key (vector-length table)))
     (binding (hash-binding table key hash-index)))
    (if (not (null? binding))
      (set-car! (cdr binding) value)
      (vector-set!
        table
        hash-index
        (cons
          (list key value)
          (vector-ref table hash-index))))))

(define (make-hash-table size)
  (let
    ((table (make-vector size)))
    (hash-table-clear! table)
    table))
