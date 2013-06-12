
;;; PascalJS - a pascal to scheme translator

;;; Author: Rick Miskowski - www.richardmiskowski.com

(define pascal-errors? #f)

(define pascal-error-list '())

(define (pascal-display-screen)
  #f) ; TODO resolve
; ((pascal-stream-out-string 'display)))

(define (sub1 n) (- n 1))

(define (last x)
  (cond
    ((null? x) x)
    ((null? (cdr x)) x)
    (else
      (last (cdr x)))))

;;; define some pascal symbols

(define semi-colon (string->symbol ";"))

(define colon (string->symbol ":"))

(define open-paren (string->symbol "("))

(define close-paren (string->symbol ")"))

(define open-bracket (string->symbol "["))

(define close-bracket (string->symbol "]"))

(define old-open-bracket (string->symbol "(."))

(define old-close-bracket (string->symbol ".)"))

(define period (string->symbol "."))

(define comma (string->symbol ","))

(define single-quote (string->symbol "'"))

(define pound (string->symbol "#"))

(define dot-dot (string->symbol ".."))

(define assignment (string->symbol ":="))

(define old-open-comment (string->symbol "(*"))

(define old-close-comment (string->symbol "*)"))

(define pointer (string->symbol "^"))

;;; define get token interface

(define (filt x)
  (display "expr: ")
  (write x) (newline)
; (my-display-pascal x) (my-newline-pascal)
  x)

(define (get-pascal-token-and-line-number)
  (let
    ((line-number (pascal-line-number)))
    (list (get-pascal-token) line-number)))

(define pascal-token-ring (list (list) (list) (list) (list)))

(define previous-pascal-token '())

(define (initialize-pascal-input)

  (initialize-pascal-lex)
  (set-cdr! (cdr (cddr pascal-token-ring)) pascal-token-ring)
  (set-car! (cdr pascal-token-ring) (get-pascal-token-and-line-number))
  (set-car! (cddr pascal-token-ring) (get-pascal-token-and-line-number))
  (set-car! (cdr (cddr pascal-token-ring)) (get-pascal-token-and-line-number)))

(define (get-next-pascal-token)
  (set! previous-pascal-token (my-car (my-car pascal-token-ring)))
  (set-car! pascal-token-ring (get-pascal-token-and-line-number))
  (set! pascal-token-ring (cdr pascal-token-ring))
  (caar pascal-token-ring))

(define (lookahead-pascal-token)
  (car (cadr pascal-token-ring)))

(define (double-lookahead-pascal-token)
  (car (cadr (cdr pascal-token-ring))))

(define (triple-lookahead-pascal-token)
  (car (cadr (cddr pascal-token-ring))))

(define (previous-pascal-token-is-a-pointer?)
  (eq? previous-pascal-token pointer))

(define (get-line-number)
  (cadr (car pascal-token-ring)))

;;; environment

(define types-environment '())
(define vars-environment '())
(define procedures-environment '())
(define function-names-environment '())

(define (make-pascal-frame)
  (set! types-environment (cons (make-hash-table 50) types-environment))
  (set! vars-environment (cons '() vars-environment))
  (set! procedures-environment (cons '() procedures-environment))
  (set! function-names-environment (cons '() function-names-environment)))

(define (pop-frame)
  (if (not (null? types-environment))
    (set! types-environment (cdr types-environment)))
  (if (not (null? vars-environment))
    (set! vars-environment (cdr vars-environment)))
  (if (not (null? procedures-environment))
    (set! procedures-environment (cdr procedures-environment)))
  (if (not (null? function-names-environment))
    (set! function-names-environment (cdr function-names-environment))))

;;; environment for type definitions

(define (clear-vector-entry! table n)
  (if (>= n 0)
    (begin
      (vector-set! table n '())
      (clear-vector-entry! table (sub1 n)))))

(define (hash-table-clear! table)
  (if (vector? table)
    (clear-vector-entry! table (sub1 (vector-length table)))))

(define (type-ref frames type)
  ; search all frames for the type
  (if (null? frames)
    (begin
      (if (not (previous-pascal-token-is-a-pointer?))
        (log-error-message "type not found:" type)
      '()))
    (let
      ((types (car frames)))
      (let
        ((type-value (hash-ref types type)))
        (if (null? type-value)
          (type-ref (cdr frames) type)
          type-value)))))

(define (log-type-definition type value)
  ; add the type to the current frame
  (if (null? types-environment)
    (log-error "no environment")
    (let
      ((frame (car types-environment)))
      (if (null? (hash-ref frame type))
        (hash-set! frame type value)
        (log-error-message "type already defined:" type)))))

;;; environment for variable definitions

(define (variable-ref frames variable)
  ; search all frames for the variable
  (if (null? frames)
    #f
    (let
      ((variables (car frames)))
      (let
        ((variable-value (assq variable variables)))
        (if (not variable-value)
          (variable-ref (cdr frames) variable)
          variable-value)))))

(define (log-var-definition variable type)
  ; add the variable to the current frame
  (if (not (null? vars-environment))
    (set-car! vars-environment
      (cons (list variable type) (car vars-environment)))))

(define (variable-defined-in-latest-frame? variable)
  (if (null? vars-environment)
    #f
    (assq variable (car vars-environment))))

;;; environment for procedure definitions

(define (procedure-ref-in-current-frame procedure-name frame)
  (if (null? frame)
    #f
    (let
      ((procedure (car frame)))
      (if (eq? procedure-name (cadr (car procedure)))
        procedure
        (procedure-ref-in-current-frame procedure-name (cdr frame))))))

(define (procedure-defined-in-latest-frame? procedure-name)
  (if (null? procedures-environment)
    #f
    (procedure-ref-in-current-frame
      procedure-name (car procedures-environment))))

(define (procedure-ref frames procedure-name)
  (if (null? frames)
    #f
    (let
      ((frame (my-car frames)))
      (let
        ((procedure (procedure-ref-in-current-frame procedure-name frame)))
        (if procedure
          procedure
          (procedure-ref (my-cdr frames) procedure-name))))))

(define (get-formal-parameters procedure)
  (cadr procedure))

(define builtin-functions
  '((ABS abs)
    (SIN sin)
    (COS cos)
    (ARCTAN atan)
    (EXP exp)
    (LN log)
    (SQRT sqrt)
    (ROUND ROUND)
    (FLOOR FLOOR)
    (CEIL CEIL)
    (SQR SQR)
    (SUCC SUCC)
    (PRED PRED)
    (ODD ODD)
    (NOT not)))

(define (builtin-function? function)
  (assq function builtin-functions))

(define formal-parameters-for-builtin
  '((ABS ((PARM1 REAL VALUE)))
    (SIN ((PARM1 REAL VALUE)))
    (COS ((PARM1 REAL VALUE)))
    (ARCTAN ((PARM1 REAL VALUE)))
    (EXP ((PARM1 REAL VALUE)))
    (LN ((PARM1 REAL VALUE)))
    (SQRT ((PARM1 REAL VALUE)))
    (ROUND ((PARM1 REAL VALUE)))
    (FLOOR ((PARM1 REAL VALUE)))
    (CEIL ((PARM1 REAL VALUE)))
    (SQR ((PARM1 REAL VALUE)))
    (SUCC ((PARM1 INTEGER VALUE)))
    (PRED ((PARM1 INTEGER VALUE)))
    (ODD ((PARM1 INTEGER VALUE)))
    (NOT ((PARM1 BOOLEAN VALUE)))))

(define (get-formal-parameters-for-builtin function)
  (assq function formal-parameters-for-builtin))

(define (function? identifier)
  (if (builtin-function? identifier)
    #t
    (let
      ((procedure (procedure-ref procedures-environment identifier)))
      (if procedure
        (if (eq? (caar procedure) 'FUNCTION)
          procedure
          #f)
        #f))))

(define (filter-built-in-function function)
  (let
    ((builtin (assq function builtin-functions)))
    (if builtin
      (cadr builtin)
      function)))

(define (proc? identifier)
  (if (standard-procedure? identifier)
    #t
    (let
      ((procedure (procedure-ref procedures-environment identifier)))
      (if procedure
        (if (eq? (caar procedure) 'PROCEDURE)
          procedure
          #f)
        #f))))

(define (log-procedure-definition definition)
  ; add the procedure definition to the current frame
  (if (not (null? procedures-environment))
    (set-car! procedures-environment
      (cons definition (car procedures-environment)))))

;;; environment for function names

(define (current-function-name-ref)
  (if (null? function-names-environment)
    #f
    (car function-names-environment)))

(define (log-current-function-name function-name)
  (set-car! function-names-environment function-name))

; TODO orpan
; (if (not (null? vars-environment))
;   (set-car! vars-environment
;     (cons (list variable type) (car vars-environment)))))

;;; routines to log errors

(define (log-error message)
  (set! pascal-errors? #t)
  ((pascal-error-list 'add)
    (list "*** ERROR on line " (pascal-line-number) " " message))
  (my-display-pascal ";*** ERROR on line ")
  (my-display-pascal (pascal-line-number))
  (my-display-pascal " ")
  (my-display-pascal message)
  (my-newline-pascal))

(define (log-error-message message info)
  (set! pascal-errors? #t)
  ((pascal-error-list 'add)
    (list "*** ERROR on line " (pascal-line-number) " "
    message info))
  (my-display-pascal ";*** ERROR on line ")
  (my-display-pascal (pascal-line-number))
  (my-display-pascal " ")
  (my-display-pascal message)
  (my-display-pascal " ")
  (my-display-pascal info)
  (my-newline-pascal))

;;; identifier definitions

(define separators (list
  semi-colon colon open-paren close-paren open-bracket
  close-bracket old-open-bracket old-close-bracket
  period comma single-quote pound dot-dot assignment
  old-open-comment old-close-comment
  '* '< '> '= '+ '<> '<= '>=))

(define (identifier? identifier)
  (and (symbol? identifier)
       (not (memq identifier separators))))

;;; expect token routines

(define (expect-open-paren)
  (if (not (eq? (get-next-pascal-token) open-paren))
    (log-error "open paren expected")))

(define (expect-close-paren)
  (if (not (eq? (get-next-pascal-token) close-paren))
    (log-error "close paren expected")))

(define (expect-open-bracket)
  (if (not (open-bracket? (get-next-pascal-token)))
    (log-error "open bracket expected")))

(define (expect-close-bracket)
  (if (not (close-bracket? (get-next-pascal-token)))
    (log-error "close bracket expected")))

(define (expect-comma)
  (if (not (eq? (get-next-pascal-token) comma))
    (log-error "comma expected")))

(define (expect-period)
  (if (not (eq? (get-next-pascal-token) period))
    (log-error "period expected")))

(define (expect-assignment)
  (if (not (eq? (get-next-pascal-token) assignment))
    (log-error ":= expected")))

(define (expect-semi-colon)
  (if (not (eq? (get-next-pascal-token) semi-colon))
    (log-error "; expected")))

(define (expect-colon)
  (if (not (eq? (get-next-pascal-token) colon))
    (log-error ": expected")))

(define (expect-equal)
  (if (not (eq? (get-next-pascal-token) '=))
    (log-error "= expected")))

(define (expect-then)
  (if (not (eq? (get-next-pascal-token) 'THEN))
    (log-error "THEN expected")))

(define (expect-of)
  (if (not (eq? (get-next-pascal-token) 'OF))
    (log-error "OF expected")))

(define (expect-to)
  (if (not (eq? (get-next-pascal-token) 'TO))
    (log-error "TO expected")))

(define (expect-do)
  (if (not (eq? (get-next-pascal-token) 'DO))
    (log-error "DO expected")))

(define (expect-until)
  (if (not (eq? (get-next-pascal-token) 'UNTIL))
    (log-error "UNTIL expected")))

; TODO get-next-pascal-token should change old type seps to single char seps
(define (open-bracket? symbol)
  (or (eq? symbol open-bracket) (eq? symbol old-open-bracket)))

(define (close-bracket? symbol)
  (or (eq? symbol close-bracket) (eq? symbol old-close-bracket)))

;;; pascal recursive descent type definition parser

;;; parse constants

(define (parse-const-definition)
  (let
    ((name (get-next-pascal-token)))
    (expect-equal)
    (let
      ((value (get-next-pascal-token)))
      ; TODO support strings too. get-next-pascal-token will convert 'token'
      ; to a scheme string
      (if (not (number? value))
        (begin
          (log-error "number expected")
          (set! value 0)))
      (log-type-definition name (list 'NUMBER value)))))

(define (parse-const-definitions)
  (parse-const-definition)
  (if (eq? (lookahead-pascal-token) semi-colon)
    (if (not (member (double-lookahead-pascal-token)
                     '(() TYPE VAR PROCEDURE FUNCTION BEGIN)))
      (begin
        (expect-semi-colon)
        (parse-const-definitions))
      (expect-semi-colon))))

(define (parse-constant-section)
  (get-next-pascal-token) ; CONST
  (parse-const-definitions))

;;; parse types

(define (parse-comma-then-token)
  (if (eq? (lookahead-pascal-token) comma)
    (get-next-pascal-token)) ; comma
  ; TODO else error comma expected
  (get-next-pascal-token))

(define (parse-enumeration token)
  (if (not (eq? token close-paren))
    (cons token (parse-enumeration (parse-comma-then-token)))
    '()))

(define (parse-token-ignore-packed)
  (if (eq? (lookahead-pascal-token) 'PACKED)
    (get-next-pascal-token)) ; PACKED
  (get-next-pascal-token))

(define (constant? token)
  (and (identifier? token)
       (identifier-is-constant? token)))

(define (number-or-constant? token)
  (or (and (eq? token '-)
           (or (number? (lookahead-pascal-token))
               (constant? (lookahead-pascal-token))))
      (number? token)
      (constant? token)))

(define (parse-number token)
  (let
    ((num token)
     (negate #f))
    (if (eq? token '-)
      (begin
        (set! negate #t)
        (set! num (get-next-pascal-token))))
    (if (identifier? num)
      (set! num (get-number num)))
    (if (not (number? num))
      (begin
        (log-error "number or constant identifier expected")
        (set! num 0)))
    (if negate
      (* num -1)
      num)))

(define (parse-simple-type token)
  (cond
    ((number-or-constant? token)
      (let
        ((num (parse-number token)))
        (if (eq? (lookahead-pascal-token) dot-dot)
          (let
            ((dummy (get-next-pascal-token)) ; ..
             (second-number (parse-number (get-next-pascal-token))))
            (list 'SUBRANGE (make-subrange num second-number)))
          (list 'NUMBER num))))
    ((eq? token open-paren)
      (list 'ENUMERATION (make-enumeration (parse-enumeration (get-next-pascal-token)))))
    (else
      (list 'TYPE token))))

(define (another-index?)
  (cond
    ((and
      (close-bracket? (lookahead-pascal-token))
      (open-bracket? (double-lookahead-pascal-token)))
      (get-next-pascal-token)
      (get-next-pascal-token)
      #t)
    ((eq? (lookahead-pascal-token) comma)
      (get-next-pascal-token)
      #t)
    (else
      #f)))

(define (parse-indices)
  (let
    ((index (parse-simple-type (get-next-pascal-token))))
    (if (another-index?)
      (list index (cons 'ARRAY (parse-indices)))
      (begin
        (expect-close-bracket)
        (get-next-pascal-token) ; OF
        (list index (parse-type))))))

(define (parse-array)
  (expect-open-bracket)
  (cons 'ARRAY (parse-indices)))

(define (parse-bit)
  (expect-open-paren)
  (let
    ((bit (list 'BIT (get-next-pascal-token))))
    (expect-close-paren)
    bit))

(define (parse-field)
  (let
    ((name (get-next-pascal-token)))
    (expect-colon)
    (list name (parse-type))))

(define (parse-fields)
  (cond
    ((or (eq? (lookahead-pascal-token) 'END)
         (eq? (lookahead-pascal-token) close-paren)) ; end of record
      '())
    ((eq? (lookahead-pascal-token) 'CASE)
      (get-next-pascal-token)
      (parse-variant-part))
    (else
      (let
        ((field (parse-field)))
        (if (eq? (lookahead-pascal-token) semi-colon)
          (expect-semi-colon))
        (cons field (parse-fields))))))

(define (parse-tag)
  (let
    ((tag '()))
    (if (eq? (double-lookahead-pascal-token) colon)
      (begin
        (set! tag (get-next-pascal-token))
        (get-next-pascal-token) ; colon
        (list 'TAG tag))
      (list 'TAG))))

(define (parse-case-label-list)
  (let
    ((rest '())
     (index (parse-simple-type (get-next-pascal-token))))
    (if (eq? (lookahead-pascal-token) comma)
      (begin
        (get-next-pascal-token)
        (set! rest (parse-case-label-list))))
    (cons index rest)))

(define (parse-variant)
  (let
    ((case-label-list (parse-case-label-list))
     (colon (expect-colon))
     (open-paren (expect-open-paren))
     (field-list (parse-fields))
     (close-paren (expect-close-paren)))
    (list case-label-list field-list)))

(define (parse-variants)
  (cond
    ((or (eq? (lookahead-pascal-token) 'END)
         (eq? (lookahead-pascal-token) close-paren)) ; end of variant
      '())
    (else
      (let
        ((variant (parse-variant)))
        (if (eq? (lookahead-pascal-token) semi-colon)
          (expect-semi-colon))
        (cons variant (parse-variants))))))

(define (parse-variant-part)
  (let
    ((tag (parse-tag))
     (type (get-next-pascal-token))
     (of (expect-of)))
    (list 'VARIANT tag type (parse-variants))))

(define (parse-record)
  (let
    ((fields (cons 'RECORD (parse-fields))))
    (get-next-pascal-token) ; END
    fields))

(define (parse-set)
  (get-next-pascal-token) ; OF
  (list 'SET (parse-simple-type (get-next-pascal-token))))

(define (parse-pointer)
  (list 'POINTER (parse-type)))

(define (parse-type)
  (let
    ((token (parse-token-ignore-packed)))
    (cond
      ((eq? token 'ARRAY)
        (parse-array))
      ((eq? token 'BIT)
        (parse-bit))
      ((eq? token 'RECORD)
        (parse-record))
      ((eq? token 'SET)
        (parse-set))
      ((eq? token pointer)
        (parse-pointer))
      (else
        (parse-simple-type token)))))

(define (parse-type-definition)
  (let
    ((name (get-next-pascal-token)))
    (expect-equal)
    (log-type-definition name (parse-type))))

(define (parse-type-definitions)
  (parse-type-definition)
  (if (eq? (lookahead-pascal-token) semi-colon)
    (if (not (member (double-lookahead-pascal-token)
                     '(() VAR PROCEDURE FUNCTION BEGIN)))
      (begin
        (expect-semi-colon)
        (parse-type-definitions))
      (expect-semi-colon))))

(define (parse-type-section)
  (get-next-pascal-token) ; TYPE
  (parse-type-definitions))

(define (get-eof)
  (if (not (null? (get-next-pascal-token)))
    (get-eof)))

;;; parse variables

(define (update-offset! ref offset)
  (set-cdr! (last (car (cddr ref))) (list offset)))

(define (parse-array-index)
  (parse-expression '()))

(define (parse-array-variable ref input-path)
  (get-next-pascal-token) ; either open bracket or comma
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (if (not (array? type))
      (log-error "array type expected"))
    (let*
      ((next-type (car (cddr input-path)))
       (expression (parse-array-index))
       (first (get-first-range type-value))
       (last (get-last-range type-value))
       (next-type-size (get-type-size next-type))
       (offset (list '$offset expression first last next-type-size)))
      (update-offset! ref offset)
      (if (eq? (lookahead-pascal-token) close-bracket)
        (get-next-pascal-token))
      (let
        ((token (lookahead-pascal-token))
         (input-path (get-type-of-type next-type)))
        (cond
          ((or (eq? token open-bracket) (eq? token comma))
            (set! ref (parse-array-variable ref input-path)))
          ((eq? token pointer)
            (set! ref (parse-pointer-variable ref token input-path)))
          ((eq? token period)
            (set! ref (parse-record-variable ref token input-path))))
        ref))))

(define (parse-record-variable ref token input-path)
  (get-next-pascal-token) ; period
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (if (not (record? type))
      (log-error "record type expected"))
    (let*
      ((field-name (lookahead-pascal-token))
       (offset 0))
      (if (not (identifier? field-name))
        (log-error "record field identifier expected"))
      (set! offset
        (get-record-type-up-to-field-size (cdr input-path) field-name))
      (update-offset! ref offset)
      (set! ref
        (parse-variable
          ref
          (get-next-pascal-token)
          (get-record-field-type (cdr input-path) field-name)))
      ref)))

(define (parse-pointer-variable ref token input-path)
  (get-next-pascal-token) ; pointer
  (if (not (pointer? (car input-path)))
    (log-error "pointer type expected"))
  (let
    ((token (lookahead-pascal-token))
     (input-path (get-type-of-type (cadr input-path))))
    (set! ref (list 'vector-ref ref (list '+)))
    (cond
      ((eq? token open-bracket)
        (set! ref (parse-array-variable ref input-path)))
      ((eq? token period)
        (set! ref (parse-record-variable ref token input-path))))
    ref))

(define (parse-variable ref token input-path)
  (let
    ((input-path (get-type-of-type input-path)))
    (let
      ((next-token (lookahead-pascal-token)))
      (cond
        ((eq? next-token open-bracket)
          (set! ref (parse-array-variable ref input-path)))
        ((eq? next-token period)
          (set! ref (parse-record-variable ref token input-path)))
        ((eq? next-token pointer)
          (set! ref (parse-pointer-variable ref token input-path))))
      ref)))

;;; parse expressions

(define expression-operators '(+ - * / DIV MOD OR AND = <> < <= >= >))

(define (variable->type variable)
  (let
    ((type (variable-ref vars-environment variable)))
    (if (not type)
      (begin
        (log-error-message "undeclared identifier" variable)
        'INTEGER) ; default
      (cadr type))))

(define (infix-to-prefix expr)

  (letrec (

    (precedence
      (lambda (operator)
        (case operator
          ((+ -) 3)
          ((* / DIV MOD) 4)
          ((OR) 0)
          ((AND) 1)
          ((= <> < <= >= >) 2))))
           ; else TODO invalid operator

    (infix-aux
      (lambda (expr operators operands)
        (infix-iter
          (my-cdr expr)
          operators
          (cons (infix-to-prefix (my-car expr)) operands))))

    (infix-iter
      (lambda (expr operators operands)
        (cond
          ((and (null? expr) (null? operators))
            (my-car operands))
          ((and
            (not (null? expr))
            (or
              (null? operators)
              (> (precedence (my-car expr)) (precedence (my-car operators)))))
                (infix-aux
                  (my-cdr expr)
                  (cons (my-car expr) operators)
                  operands))
          (else
            (infix-iter
              expr
              (my-cdr operators)
              (cons
                (list (my-car operators)
                      (cadr operands) (my-car operands))
                (cddr operands))))))))
    
    (if (or (null? expr)
            (number? expr)
            (string? expr)
            (char? expr)
            (boolean? expr)
           ;(and (symbol? expr) (not (memq expr expression-operators)))
            (and (pair? expr)
                 (symbol? (my-car expr))
                 (not (memq (my-car expr) expression-operators))))
      expr
      (infix-aux expr '() '()))))

(define (parse-infix-simple-expression level)
  (letrec (
    (factors '())
    (push-level
      (lambda (kind)
        (set! level (cons kind level))))
    (pop-level
      (lambda ()
        (if (null? level)
          #f
          (set! level (cdr level)))))
    (level-is-function?
      (lambda ()
        (eq? (car level) 'FUNC)))
    (save
      (lambda (factor)
        (set! factors (cons factor factors))))
    (next-is-open-paren?
      (lambda ()
        (eq? (lookahead-pascal-token) open-paren)))
    (next-is-close-paren?
      (lambda ()
        (eq? (lookahead-pascal-token) close-paren)))
    (next-is-number-or-id?
      (lambda ()
        (or (number? (lookahead-pascal-token))
            (identifier? (lookahead-pascal-token))
            (string? (lookahead-pascal-token))
            (char? (lookahead-pascal-token)))))
    (next-is-op?
      (lambda ()
        (memq (lookahead-pascal-token) expression-operators)))
    (parse-number-or-variable
      (lambda ()
        (let
          ((token (get-next-pascal-token)))
          (set! token
            (cond
              ((number? token) token)
              ((string? token) token)
              ((char? token) token)
              ((eq? token 'FALSE) '#f)
              ((eq? token 'TRUE) '#t)
              ((eq? token 'NIL) ; nil is a constant pointer type value
                (list 'quote '()))
              ((and (variable-identifier?) ; variable has simple type
                    (not (function? token)))
                (if (variable-ref vars-environment token)
                  (list 'vector-ref token 0)
                  ; identifier must be a constant
                  (identifier-is-constant? token))) ; returns number value or error
              ((identifier? token)
                (if (builtin-function? token)
                  (if (and (eq? token 'NOT)
                           (not (eq? (lookahead-pascal-token) open-paren)))
                    (let
                      ((token (get-next-pascal-token)))
                      (list 'not
                        (parse-variable
                          (list 'vector-ref token (list '+))
                          token
                          (type-ref types-environment (variable->type token)))))
                    (append
                      (list (filter-built-in-function token))
                      (parse-builtin-function-actual-parameters
                        (get-formal-parameters-for-builtin token))))
                  (if (function? token)
                    (append
                      (list token)
                      (parse-function-actual-parameters
                        (get-formal-parameters
                          (procedure-ref procedures-environment token))))
                    (parse-variable
                      (list 'vector-ref token (list '+))
                      token
                      (type-ref types-environment (variable->type token))))))
              (else ; there had to be a number or identifier
                0)))
          token))) ; default
    (parse-function-actual-parameters
      (lambda (formal-parameters)
        (if (= (length formal-parameters) 0)
          '() ; function without arguments
          (let
            ((expression-list '()))
            (expect-open-paren)
            (push-level 'FUNC)
            (set! expression-list
              (parse-expression-list formal-parameters level))
            (expect-close-paren)
            (pop-level) ; TODO check false return
            expression-list))))
    (parse-builtin-function-actual-parameters
      (lambda (formal-parameters)
        (if (= (length formal-parameters) 0)
          '() ; function without arguments
          (let
            ((expression-list '()))
            (expect-open-paren)
            (push-level 'FUNC)
            (set! expression-list
              (parse-expression-list-for-builtin-function
                formal-parameters level))
            (expect-close-paren)
            (pop-level) ; TODO check false return
            expression-list))))
    (state-0
      (lambda ()
        (cond
          ((next-is-number-or-id?)
            (save (parse-number-or-variable))
            (state-1))
          ((next-is-open-paren?)
            (save (get-next-pascal-token))
            (push-level 'EXPR)
            (state-0))
          (else
            (state-4)))))
    (state-1
      (lambda ()
        (cond
          ((next-is-op?)
            (save (get-next-pascal-token))
            (state-2))
          ((next-is-close-paren?)
            (if (null? level)
              (state-4)
              (if (not (level-is-function?))
                (begin
                  (save (get-next-pascal-token))
                  (if (pop-level)
                    (state-1)
                    (begin
                      (log-error "incomplete expression")
                      (set! factors (list 0))
                      (state-4))))
                 (state-4))))
          (else
            (state-4)))))
    (state-2
      (lambda ()
        (cond
          ((next-is-number-or-id?)
            (save (parse-number-or-variable))
            (state-1))
          ((next-is-open-paren?)
            (save (get-next-pascal-token))
            (push-level 'EXPR)
            (state-0))
          (else
            (state-3)))))
    (state-3
      (lambda ()
        (log-error "bad expression")
        (state-4)))
    (state-4 ; done
      (lambda ()
        '())))
    (state-0)
    (if (null? factors)
      (begin
        (log-error "missing expression")
        (set! factors (list 0))))
    (reverse factors)))

(define (get-parse-tree-of-infix-expression expr)
  (letrec (
    (get-next
      (lambda ()
        (if (null? expr)
          '()
          (let ((next (car expr)))
            (set! expr (cdr expr))
            next))))
    (parse
      (lambda ()
        (let ((token (get-next)))
          (cond
            ((null? token) '())
            ((eq? token open-paren)
              (cons (parse) (parse)))
            ((eq? token close-paren)
              '())
            (else
              (cons token (parse))))))))
    (parse)))

(define (parse-simple-expression level)
  (infix-to-prefix
    (get-parse-tree-of-infix-expression (parse-infix-simple-expression level))))

(define (simplify-expression expression)
  (letrec (

    (all-numeric?
      (lambda (expression)
        (cond
          ((null? expression) #t)
          ((number? (car expression))
            (all-numeric? (cdr expression)))
          (else
            #f))))

    (sum-numerics
      (lambda (expression)
        (cond
          ((null? expression) 0)
          (else
            (+ (car expression) (sum-numerics (cdr expression)))))))

    (diff-numerics
      (lambda (expression)
        (cond
          ((null? expression) 0)
          (else
            (- (car expression) (diff-numerics (cdr expression)))))))

    (filter-and-or
      (lambda (expression)
        (cond
          ((eq? expression 'AND) 'and)
          ((eq? expression 'OR) 'or)
          (else
            expression))))

    (simplify
      (lambda (expression)
        (cond
          ((null? expression) '())
          ((or (number? expression)
               (symbol? expression)
               (string? expression)
               (char? expression))
            expression)
          ((pair? (car expression))
            (cons (simplify (car expression)) (simplify (cdr expression))))
          ((eq? (car expression) '+)
            (if (null? (cdr expression))
              0
              (if (all-numeric? (cdr expression))
                (sum-numerics (cdr expression))
                (cons (car expression) (simplify (cdr expression))))))
          ((eq? (car expression) '-)
            (if (null? (cdr expression))
              0
              (if (all-numeric? (cdr expression))
                (diff-numerics (cdr expression))
                (cons (car expression) (simplify (cdr expression))))))
          (else
            (cons (filter-and-or (car expression))
                  (simplify (cdr expression))))))))

    (let
      ((simplified-expression (simplify expression)))
      (if (equal? expression simplified-expression)
        expression
        (simplify-expression simplified-expression)))))

(define relops '(= <> < <= >= >))

(define (display-x x)
  (my-display-pascal "parse-variable: ")
  (my-display-pascal x) (my-newline-pascal)
  x)

(define (parse-expression level)
  ; TODO add type parameter this routine will check result matches type
  (parse-simple-expression level))

(define (parse-expression-list formal-parameters level)
  (if (null? formal-parameters)
    (begin
      (log-error "wrong number of actual arguments")
      '())
    (cons
      (list '$make-parm (parse-expression level))
      (if (eq? (lookahead-pascal-token) comma)
        (begin
          (get-next-pascal-token) ; comma
          (parse-expression-list (cdr formal-parameters) level))
        '()))))

(define (parse-expression-list-for-builtin-function formal-parameters level)
  (if (null? formal-parameters)
    (begin
      (log-error "wrong number of actual arguments")
      '())
    (cons
      (parse-expression level)
      (if (eq? (lookahead-pascal-token) comma)
        (begin
          (get-next-pascal-token) ; comma
          (parse-expression-list-for-builtin-function
            (cdr formal-parameters) level))
        '()))))

(define (parse-expression-list-no-formals level)
  (cons
    (parse-expression level)
    (if (eq? (lookahead-pascal-token) comma)
      (begin
        (get-next-pascal-token) ; comma
        (parse-expression-list-no-formals level))
      '())))

;;; parse statements

(define (parse-procedure-actual-parameters formal-parameters)
  (if (= (length formal-parameters) 0)
    '() ; procedure without arguments
    (let
      ((expression-list '()))
      (expect-open-paren)
      (set! expression-list
        (parse-expression-list formal-parameters '()))
      (expect-close-paren)
      expression-list)))

(define (variable-identifier?)
  (let
    ((next-token (lookahead-pascal-token)))
    (not (or 
      (eq? next-token open-paren)
      (eq? next-token open-bracket)
      (eq? next-token period)
      (eq? next-token pointer)))))

(define (replace-outer-vector-ref-with-vector-set! x)
  ; TODO should verify that there is a vector-ref there
  (cons 'vector-set! (cdr x)))

(define (standard-procedure? procedure-name)
  (memq procedure-name '(WRITE WRITELN READ NEW)))

(define (parse-standard-procedure procedure-name)
  (case procedure-name
    ((WRITE) (parse-write))
    ((WRITELN) (parse-writeln))
    ((READ) (parse-read))
    ((NEW) (parse-new))))

(define (display-expressions expressions type)
  (if (null? expressions)
    (if (eq? type 'WRITELN)
      (list (list 'newline))
      '())
    (let
      ((expression (car expressions)))
      (let
        ((display-expression (list 'display expression)))
        (cons display-expression
              (display-expressions (cdr expressions) type))))))

(define (parse-write)
  (let
    ((expression-list '()))
    (expect-open-paren)
    (set! expression-list (parse-expression-list-no-formals '()))
    (expect-close-paren)
    (append (list 'begin) (display-expressions expression-list 'WRITE))))

(define (parse-writeln)
  (if (eq? (lookahead-pascal-token) open-paren)
    (let
      ((expression-list '()))
      (expect-open-paren)
      (set! expression-list (parse-expression-list-no-formals '()))
      (expect-close-paren)
      (append (list 'begin) (display-expressions expression-list 'WRITELN)))
    (list 'newline)))

(define (parse-read)
  (expect-open-paren)
  (let
    ((token (get-next-pascal-token)))
    (let
      ((type (type-ref types-environment (variable->type token)))
       (read-function 'read))
      (if (eq? (get-type-of-type (variable->type token)) 'CHAR)
        (set! read-function 'read-char))
      (let
        ((variable
          (append
            (replace-outer-vector-ref-with-vector-set!
              (parse-variable
                (list 'vector-ref token (list '+))
                token
                type))
            (list (list read-function)))))
        (expect-close-paren)
        variable))))

(define (parse-new)
  (expect-open-paren)
  (let
    ((variable (get-next-pascal-token)))
    (if (not (identifier? variable))
      (log-error "identifier expected"))
    (let
      ((variable-type (get-type (variable->type variable))))
      (if (not (pointer? (car variable-type)))
        (log-error "pointer type expected"))
      (let
        ((type-size (get-type-size (cadr variable-type))))
        (expect-close-paren)
        (list 'vector-set! variable 0
          (list 'make-vector type-size)))))) 

(define (parse-procedure-statement procedure-name)
  (if (standard-procedure? procedure-name)
    (parse-standard-procedure procedure-name)
    (append
      (list procedure-name)
      (parse-procedure-actual-parameters
        (get-formal-parameters
          (procedure-ref procedures-environment procedure-name))))))

(define (expect-assignment-parse-expression)
  (expect-assignment)
  (parse-expression '()))

(define (parse-assign-string-to-var variable str)
  (list '$string-init variable str (- (string-length str) 1)))
  ; TODO verify length of string is same as array of char subrange type

(define (parse-assignment-statement variable)
  (if (variable-identifier?)
    (begin
      (if (proc? variable) ; a procedure statement with no parameters
        (parse-procedure-statement variable)
        (if (variable-ref vars-environment variable)
          (begin
            (expect-assignment)
            (if (string? (lookahead-pascal-token)) ; assign a string to a var
              (parse-assign-string-to-var variable (get-next-pascal-token))
              (list 'vector-set! variable 0 (parse-expression '()))))
          (let
            ((ref (current-function-name-ref)))
            (if (and ref (eq? (car ref) variable))
              ; expect function to simply return here
              (expect-assignment-parse-expression)
              (log-error-message "undeclared variable:" variable))))))
    (if (eq? (lookahead-pascal-token) open-paren) ; a procedure statement
      (begin
        (if (proc? variable)
          (parse-procedure-statement variable)
          (log-error "procedure expected")))
      (append
        (replace-outer-vector-ref-with-vector-set! (parse-variable
          (list 'vector-ref variable (list '+))
          variable
          (type-ref types-environment (variable->type variable))))
        (list (expect-assignment-parse-expression))))))

(define (parse-if-statement)
  (let
    ((expr (parse-expression '())))
    (expect-then)
    (let
      ((then-statement (parse-statement)))
      (if (eq? (lookahead-pascal-token) 'ELSE)
        (begin
          (get-next-pascal-token) ; ELSE
          (list 'if expr then-statement (parse-statement)))
        (list 'if expr then-statement)))))

(define (parse-case-statement) '())

(define (parse-repeat-statement)
  (let
    ((sequence (parse-statement-sequence)))
    (expect-until)
    (let
      ((expression (parse-expression '())))
      (list 'let 'loop '()
        sequence
        (list 'if (list 'not expression) (list 'loop))))))

(define (parse-while-statement)
  (let
    ((expression (parse-expression '())))
    (expect-do)
    (let
      ((statement (parse-statement)))
      (list 'let 'loop '()
        (list 'if expression
          (list 'begin
            statement
            (list 'loop)))))))

(define (parse-for-statement)
  (let
    ((variable-name (get-next-pascal-token)))
    (let
      ((control-variable
        (parse-variable
          (list 'vector-ref variable-name (list '+))
          variable-name
          (type-ref types-environment (variable->type variable-name)))))
      (expect-assignment)
      (let
        ((initial-value (parse-expression '())))
        (expect-to)
        (let
          ((final-value (parse-expression '())))
          (expect-do)
          (let
            ((statement (parse-statement)))
            (list
              'begin
              (append (replace-outer-vector-ref-with-vector-set!
                        control-variable)
                (list initial-value))
              (list 'let 'loop '()
                (list 'if (list '<= control-variable final-value)
                  (list 'begin
                    statement
                    (append (replace-outer-vector-ref-with-vector-set!
                              control-variable)
                      (list (list '+ control-variable 1)))
                    (list 'loop)))))))))))

(define (parse-statement)
  (let
    ((token (get-next-pascal-token)))
    (cond
      ((eq? token 'BEGIN) (parse-compound-statement token))
      ((eq? token 'IF) (parse-if-statement))
      ((eq? token 'CASE) (parse-if-statement))
      ((eq? token 'WHILE) (parse-while-statement))
      ((eq? token 'REPEAT) (parse-repeat-statement))
      ((eq? token 'FOR) (parse-for-statement))
      ((identifier? token) (parse-assignment-statement token))
      (else
        (log-error "bad statement")))))

(define (append-statements)
  (let
    ((statement (parse-statement)))
    (cons statement (parse-statements))))

(define (parse-statements)
  (if (eq? (lookahead-pascal-token) semi-colon)
    (begin
      (get-next-pascal-token) ; semicolon
      (append-statements))
    '()))

; TODO recode to remove redundant nested begin blocks
(define (expand-imbedded-expressions! statements)
  (if (not (null? statements))
    (let
      ((next-statement (cdr statements)))
      (if (not (null? next-statement))
        (if (and (pair? (car next-statement))
                 (eq? (caar next-statement) '$EXPRS))
          (let
            ((next-next-statement (cdr next-statement))
             (first-expr (cdar next-statement))
             (last-expr (last (car next-statement))))
            (set-cdr! statements first-expr)
            (set-cdr! last-expr next-next-statement)
            (set-car! next-statement '())
            (set-cdr! next-statement '())
            (expand-imbedded-expressions! last-expr))
          (expand-imbedded-expressions! (cdr statements)))))))

(define (parse-compound-statement token)
  (let
    ((compound-statement '()))
    (if (not (eq? token 'BEGIN))
      (log-error "BEGIN expected"))
    (if (not (eq? (lookahead-pascal-token) 'END))
      (set! compound-statement
        (cons 'begin (append-statements)))
      (set! compound-statement (list 'begin '#f)))
    (if (not (eq? (get-next-pascal-token) 'END))
      ; instead of skipping to program end - skip past next semicolon
      (log-error "END expected"))
    compound-statement))

(define (parse-statement-sequence)
  (cons 'begin (append-statements)))

;;; parse variable definitions

(define (parse-variable-definition)
  (let
    ((name (get-next-pascal-token)))
    (expect-colon)
    (let
      ((type (get-next-pascal-token)))
      (if (not (and (identifier? name) (identifier? type)))
        (log-error "identifier expected")
        (let
          ((reference (type-ref types-environment type)))
          (if (not reference)
            (log-error-message "undefined type reference" type)
            (if (variable-defined-in-latest-frame? name)
              (log-error-message "variable already defined:" name)
              (begin
                (log-var-definition name type)
                (list name
                  (list 'make-vector (get-type-size reference)))))))))))

(define (parse-variable-definitions)
  (let
    ((variable-definition (parse-variable-definition)))
    (if (eq? (lookahead-pascal-token) semi-colon)
      (if (not (member (double-lookahead-pascal-token)
                       '(() PROCEDURE FUNCTION BEGIN)))
        (begin
          (expect-semi-colon)
          (cons variable-definition (parse-variable-definitions)))
        (begin
          (expect-semi-colon)
          (list variable-definition)))
      (list variable-definition))))

(define (parse-variable-section)
  (get-next-pascal-token) ; VAR
  (parse-variable-definitions))

;;; parse procedure definitions

(define (parse-formal-parameter)
  (let
    ((passing-type 'VALUE))
    (if (eq? (lookahead-pascal-token) 'VAR)
      (set! passing-type (get-next-pascal-token)))
    (let
      ((formal-parameter (get-next-pascal-token)))
      (if (not (identifier? formal-parameter))
        (log-error "formal parameter identifier expected"))
      (expect-colon)
      (let
        ((type (get-next-pascal-token)))
        (if (not (identifier? type))
          (log-error "type identifier expected")
          (if (not (type-ref types-environment type))
            (log-error-message "undefined type reference" type)))
        (log-var-definition formal-parameter type)
        (list formal-parameter type passing-type)))))

(define (parse-formal-parameters)
  (cons
    (parse-formal-parameter)
    (if (eq? (lookahead-pascal-token) semi-colon)
      (begin
        (get-next-pascal-token)
        (parse-formal-parameters))
      '())))

(define (get-formal-parameter-names parameters)
  (if (null? parameters)
    '()
    (cons (caar parameters) (get-formal-parameter-names (cdr parameters)))))

(define (parse-procedure)
  (get-next-pascal-token) ; PROCEDURE
  (let
    ((procedure-name (get-next-pascal-token))
     (formal-parameters '()))
    (if (not (identifier? procedure-name))
      (log-error "identifier expected"))
    (if (procedure-defined-in-latest-frame? procedure-name)
      (log-error-message "procedure already defined" procedure-name))
    (if (eq? (lookahead-pascal-token) open-paren)
      (begin
        (get-next-pascal-token) ; open paren
        (set! formal-parameters (parse-formal-parameters))
        (expect-close-paren)))
    (expect-semi-colon)
    (log-procedure-definition
      (list (list 'PROCEDURE procedure-name) formal-parameters))
    (list procedure-name
      (list 'lambda (get-formal-parameter-names formal-parameters)
        (parse-block #f)))))

(define (parse-function)
  (get-next-pascal-token) ; FUNCTION
  (let
    ((function-name (get-next-pascal-token))
     (formal-parameters '()))
    (if (not (identifier? function-name))
      (log-error "identifier expected"))
    ; TODO if variable has name - error
    (if (procedure-defined-in-latest-frame? function-name)
      (log-error-message "procedure already defined" function-name))
    (if (eq? (lookahead-pascal-token) open-paren)
      (begin
        (get-next-pascal-token) ; open paren
        (set! formal-parameters (parse-formal-parameters))
        (expect-close-paren)))
    (expect-colon)
    (let
      ((function-type (get-next-pascal-token)))
      (if (not (identifier? function-type))
        (log-error "identifier expected"))
      (if (not (type-ref types-environment function-type))
        (log-error-message "undefined type reference" function-type))
      (expect-semi-colon)
      (log-procedure-definition
        (list (list 'FUNCTION function-name function-type) formal-parameters))
      (list function-name
        (list 'lambda (get-formal-parameter-names formal-parameters)
          (parse-block (list function-name function-type)))))))

(define (parse-procedure-section)
  (if (memq (lookahead-pascal-token) '(PROCEDURE FUNCTION))
    (let
      ((procedure '()))
      (if (eq? (lookahead-pascal-token) 'PROCEDURE)
        (set! procedure (parse-procedure))
        (if (eq? (lookahead-pascal-token) 'FUNCTION)
          (set! procedure (parse-function))))
      (expect-semi-colon)
      (cons procedure (parse-procedure-section)))
    '()))

;;; parse block

(define (parse-block this-function)
  (make-pascal-frame)
  (log-current-function-name this-function)
  (let
    ((variables #f)
     (procedures #f)
     (compound-statement '())
     (block '()))
    ; TODO label declarations
    (if (eq? (lookahead-pascal-token) 'CONST)
      (parse-constant-section))
    (if (eq? (lookahead-pascal-token) 'TYPE)
      (parse-type-section))
    (if (eq? (lookahead-pascal-token) 'VAR)
      (set! variables (parse-variable-section)))
    (if (memq (lookahead-pascal-token) '(PROCEDURE FUNCTION))
      (set! procedures (parse-procedure-section)))
    (set! compound-statement (parse-compound-statement (get-next-pascal-token)))
    (if variables
      (if procedures
        (set! block
          (list 'let variables (list 'letrec procedures compound-statement)))
        (set! block
          (list 'let variables compound-statement)))
      ; else no variables
      (if procedures
        (set! block
          (list 'letrec procedures compound-statement))
        (set! block compound-statement)))
    (pop-frame)
    block))

; define global routines

(define (define-global-routines block)
  (list 'letrec
    (list
      '(<>
         (lambda (a b)
           (not (eqv? a b))))
      '($offset
         (lambda (n first last size)
           (if (or (< n first) (> n last))
             (begin
               (display "array bound error: ")
               (display n) (display " out of range")
               (exit 0))
             (* (- n first) size))))
      '($string-init
         (lambda (var str len)
           (if (>= len 0)
             (begin
               (vector-set! var len (string-ref str len))
               ($string-init var str (- len 1))))))
      '(ROUND
         (lambda (n)
           (inexact->exact (round n))))
      '(FLOOR
         (lambda (n)
           (inexact->exact (floor n))))
      '(CEIL
         (lambda (n)
           (inexact->exact (ceiling n))))
      '(SQR
         (lambda (n)
           (* n n)))
      '(DIV
         (lambda (i j)
           (quotient i j)))
      '(MOD
         (lambda (i j)
           (remainder i j)))
      '(SUCC
         (lambda (i)
           (+ i 1)))
      '(PRED
         (lambda (i)
           (- i 1)))
      '(ODD
         (lambda (i)
           (= (remainder i 2) 1)))
      '($make-parm
         (lambda (parm)
           (let
             ((box (make-vector 1)))
             (vector-set! box 0 parm)
             box))))
      block))

(define (parse-pascal)
  (set! pascal-errors? #f)

  (initialize-pascal-input)
  (set! pascal-error-list (make-linked-list))

  (if (not (eq? (get-next-pascal-token) 'PROGRAM))
    (log-error "PROGRAM expected"))
  (if (not (identifier? (get-next-pascal-token)))
    (log-error "identifier expected"))
  (expect-semi-colon)

  ;;; define builtin types at top level

  (make-pascal-frame)
  (log-type-definition 'BOOLEAN
    (list 'ENUMERATION (make-enumeration '(FALSE TRUE))))
  (log-type-definition 'INTEGER (list 'SUBRANGE (make-subrange -32768 32767)))
  (log-type-definition 'REAL (list 'SUBRANGE (make-subrange 0 0))) ; TODO
  (log-type-definition 'CHAR (list 'SUBRANGE (make-subrange 0 255)))

  (let
    ((scheme-code
      (simplify-expression
        (define-global-routines (parse-block #f)))))

    (pop-frame)

    (expect-period)
    (if (not (null? (lookahead-pascal-token)))
      (log-error "rest of program ignored"))
    (get-eof)

    (if pascal-errors?
      (pascal-display-screen) ; TODO clean this up
      (my-display (scheme-eval scheme-code))) ; use eval if available

    (flush-output)))

(define (filter-display-scheme x)
  (write (list->string x)) (newline)
  x)

(define (filter-out-sublists s) ; list has empty lists. filter them out
  (if (null? s)
    '()
    (if (char? (car s))
      (cons (car s) (filter-out-sublists (cdr s)))
      (filter-out-sublists (cdr s)))))

;;; type access procedures

(define (simple-type? type)
  (memq type '(NUMBER ENUMERATION SUBRANGE BIT)))

(define (array? type)
  (eq? type 'ARRAY))

(define (record? type)
  (eq? type 'RECORD))

(define (variant-record? type)
  (eq? type 'VARIANT))

(define (set? type)
  (eq? type 'SET))

(define (type? type)
  (eq? type 'TYPE))

(define (pointer? type)
  (eq? type 'POINTER))

(define (identifier-is-constant? identifier)
  ; returns the numeric value if true
  (cond
    ((number? identifier) identifier)
    ((symbol? identifier)
      (identifier-is-constant? (get-type identifier)))
    ((pair? identifier)
      (let
        ((type (car identifier))
         (type-value (cadr identifier)))
        (cond
          ((eq? type 'NUMBER) type-value)
          ((eq? type 'TYPE) (identifier-is-constant? (get-type type-value)))
          (else
            #f))))
    (else
      #f)))

(define (get-number n)
  (let
    ((num (identifier-is-constant? n)))
    (if (not num)
      (log-error-message "unknown number type" n)
      num)))

(define (get-type type)
  (type-ref types-environment type))

(define (get-type-of-type input-path)
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (if (type? type)
      (get-type-of-type (get-type type-value))
      input-path)))
    
(define (get-first-range input-path)
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (case type
      ((NUMBER) type-value)
      ((SUBRANGE)
        (get-number ((type-value 'get-first))))
      ((ENUMERATION)
        0)
      ((TYPE)
        (get-first-range (get-type type-value)))
      (else
        (log-error "unknown range type")))))
    
(define (get-last-range input-path)
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (case type
      ((NUMBER) type-value)
      ((SUBRANGE)
        (get-number ((type-value 'get-last))))
      ((ENUMERATION)
        ((type-value 'get-last)))
      ((TYPE)
        (get-last-range (get-type type-value)))
      (else
        (log-error "unknown range type")))))

(define (set->array input-path)
  (list 'ARRAY (car input-path) (list 'TYPE 'BOOLEAN)))

;;; get type size

(define (get-array-type-size first last input-path)
  (let
    ((elements (+ (- last first) 1)))
    (* elements (get-type-size input-path))))

(define (get-variant-tag-size tag)
  (if (pair? (cdr tag))
    1 ; tag is a simple type
    0)) ; tagless

(define (variant-record-contains-field? input-path input-field-name)
  (if (null? input-path)
    #f
    (if (record-contains-field? (cadr (car input-path)) input-field-name)
      #t
      (variant-record-contains-field? (cdr input-path) input-field-name))))

(define (record-contains-field? input-path input-field-name)
  (if (null? input-path)
    #f
    (if (variant-record? (car input-path))
      (variant-record-contains-field?
        (car (cddr (cdr input-path))) input-field-name)
      (let
        ((field-name (caar input-path)))
        (if (eq? field-name input-field-name)
          #t
          (record-contains-field? (cdr input-path) input-field-name))))))

(define (get-variant-given-field input-path input-field-name)
  (if (null? input-path)
    #f
    (let
      ((this-variant-record (cadr (car input-path))))
      (if (record-contains-field? this-variant-record input-field-name)
        this-variant-record
        (get-variant-given-field (cdr input-path) input-field-name)))))

(define (get-variant-record-size input-path)
  (if (null? input-path)
    0
    (max
      (get-record-type-size
        (cadr (car input-path)))
      (get-variant-record-size (cdr input-path)))))

(define (get-field-type field input-path)
  (get-type-of-type input-path))

(define (get-variant-record-field-type input-path input-field-name)
  (if (null? input-path)
    #f
    (let
      ((field-name
        (get-record-field-type (cadr (car input-path)) input-field-name)))
      (if field-name
        field-name
        (get-variant-record-field-type (cdr input-path) input-field-name)))))

(define (get-record-field-type input-path input-field-name)
  (if (null? input-path)
    #f
    (if (variant-record? (car input-path))
      (begin
        (get-variant-record-field-type
          (car (cddr (cdr input-path)))
          input-field-name)
       )
      (let
        ((field-name (caar input-path)))
        (if (eq? field-name input-field-name)
          (get-field-type field-name (cadr (car input-path)))
          (get-record-field-type (cdr input-path) input-field-name))))))

(define (get-field-size field input-path)
  (get-type-size input-path))

(define (get-record-type-size input-path)
  (if (null? input-path)
    0
    (if (variant-record? (car input-path))
      (begin
        (+
          (get-variant-tag-size (cadr input-path))
          (get-variant-record-size
            (car (cddr (cdr input-path))))))
      (begin
        (+
          (get-field-size
            (caar input-path)
            (cadr (car input-path)))
          (get-record-type-size (cdr input-path)))))))

(define (get-record-type-up-to-field-size input-path input-field-name)
  ; size of records fields up to but not including input-field-name
  (if (null? input-path)
    0
    (if (variant-record? (car input-path))
      (begin
        (+
          (get-variant-tag-size (cadr input-path))
          (get-record-type-up-to-field-size
            (get-variant-given-field
              (car (cddr (cdr input-path)))
              input-field-name)
            input-field-name)))
      (let
        ((field-name (caar input-path)))
        (if (eq? field-name input-field-name)
          0
          (+
            (get-field-size
              field-name
              (cadr (car input-path)))
            (get-record-type-up-to-field-size
              (cdr input-path)
              input-field-name)))))))

(define (get-type-size input-path)
  (let
    ((type (car input-path))
     (type-value (cadr input-path)))
    (cond
      ((simple-type? type)
        1)
      ((array? type)
        (get-array-type-size
          (get-first-range type-value)
          (get-last-range type-value)
          (car (cddr input-path))))
      ((record? type)
        (get-record-type-size (cdr input-path)))
      ((set? type)
        (get-type-size (set->array (cdr input-path))))
      ((pointer? type)
        1)
      ((type? type)
        (get-type-size (get-type type-value)))
      (else
        (begin
          (log-error-message "unknown type" (if (symbol? type) type ""))
          0)))))

;;; start

(define (type-size type)
  (my-display-pascal type)
  (my-display-pascal " ")
  (my-display-pascal (get-type-size (type-ref types-environment type)))
  (my-newline-pascal))

(define (type-up-to-size type field-name)
  (my-display-pascal type)
  (my-display-pascal " ")
  (my-display-pascal (get-record-type-up-to-field-size
    (cdr (type-ref types-environment type)) field-name))
  (my-newline-pascal))

(define (contains-field? type field-name)
  (my-display-pascal type)
  (my-display-pascal " ")
  (my-display-pascal (record-contains-field?
    (cdr (type-ref types-environment type)) field-name))
  (my-newline-pascal))

;(define (tests)
;  (my-display-pascal (type-ref types-environment 'MAX_SIZE))
;  (my-newline-pascal)
;
;  (type-size 'INTEGER)
;  (type-size 'BT_VARIANT_ARR)
;
;  (contains-field? 'X_REC 'FIELD1)
;  (contains-field? 'BT_X_ARR 'X)
;  (contains-field? 'BT_X_ARR 'ARRAY_PART)
;  (contains-field? 'BT_X_ARR 'XXX)
;
;  (type-up-to-size 'X_REC 'FIELD1)
;  (type-up-to-size 'X_REC 'FIELD2)
;  (type-up-to-size 'X_REC 'X)
;  (type-up-to-size 'BT_X_ARR 'HEADER)
;  (type-up-to-size 'BT_X_ARR 'HEADER2)
;  (type-up-to-size 'BT_X_ARR 'BOOL_HEADER)
;  (type-up-to-size 'BT_X_ARR 'Y_HEADER)
;  (type-up-to-size 'BT_X_ARR 'X)
;  (type-up-to-size 'BT_X_ARR 'ARRAY_PART)
;  )

(define (pascal)
  (parse-pascal))
