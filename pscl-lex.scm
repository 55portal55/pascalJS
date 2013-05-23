(define (symbol->list symbol)
  (string->list (symbol->string symbol)))

(define (list->symbol symbol-list)
  (if (null? symbol-list)
    symbol-list
    (string->symbol (list->string symbol-list))))

(define (pascal-char-alpha-numeric? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))

(define lookahead-pascal-ch #\space)

(define pascal-ch #\space)

(define PASCAL-EOF-CHAR (integer->char 0))

(define PASCAL-TAB-CHAR (integer->char 9))

(define PASCAL-CARRIAGE-RETURN (integer->char 13))

(define PASCAL-LINE-FEED (integer->char 10))

(define pascal-white-space
  (list #\space #\newline
    PASCAL-TAB-CHAR PASCAL-CARRIAGE-RETURN PASCAL-LINE-FEED))

(define pascal-line-count 1)

(define (pascal-line-number)
  pascal-line-count)

(define (get-lookahead-pascal-ch)
  lookahead-pascal-ch)

(define (initialize-pascal-lex)
  (set! lookahead-pascal-ch (read-char))
  (set! pascal-ch #\space)
  (set! pascal-line-count 1))

(define MAX-TOKEN 100)

(define pascal-buffer '())

(define (get-pascal-token)
  (letrec (

    (buffer->symbol
      (lambda ()
        (string->symbol (list->string (reverse pascal-buffer)))))

    (buffer->number
      (lambda ()
        (string->number (list->string (reverse pascal-buffer)))))

    (buffer->string
      (lambda ()
        (list->string (reverse pascal-buffer))))

    (token '())

    (buffer-idx 0)

    (tail '())

    (separators
      '(#\( #\) #\[ #\] #\, #\. #\* #\< #\> #\; #\: #\= #\+ #\/ #\# #\^))

    (two-character-separators
      '((#\* #\)) (#\( #\*) (#\: #\=) (#\( #\.) (#\. #\))
        (#\. #\.) (#\< #\>) (#\< #\=) (#\> #\=) ))

    (pascal-error
      (lambda (message info)
        (my-display-pascal  "*** ERROR on line ")
        (my-display-pascal pascal-line-count)
        (my-display-pascal " ")
        (my-display-pascal message)
        (if (not (null? info))
          (begin
            (my-display-pascal " ")
            (my-display-pascal info)))
        (my-newline-pascal)))

    (read-pascal-ch
      (lambda ()
        (set! pascal-ch lookahead-pascal-ch)
      ; (my-write-pascal (char->integer pascal-ch))
      ; (my-display-pascal pascal-ch)
        (set! lookahead-pascal-ch (read-char))
        (if (eof-object? pascal-ch)
          (set! pascal-ch PASCAL-EOF-CHAR))
        (if (char=? pascal-ch #\newline)
          (begin
            (set! pascal-ch #\space)
            (set! pascal-line-count (+ pascal-line-count 1))))))
    
;   (skip-pascal-line
;     (lambda ()
;       (if (char=? pascal-ch #\newline)
;         (begin
;           (set! pascal-line-count (+ pascal-line-count 1))
;           (read-pascal-ch))
;         (begin
;           (set! pascal-ch (read-char))
;           (skip-pascal-line)))))
    
    (skip-pascal-blanks
      (lambda ()
;       (my-write-char-pascal #\*) (my-write-char-pascal pascal-ch)
        (cond
          ((char=? pascal-ch PASCAL-EOF-CHAR))
          ((memq pascal-ch pascal-white-space)
            (read-pascal-ch)
            (skip-pascal-blanks)))))

    (skip-pascal-comment
      (lambda ()
        (cond
          ((char=? pascal-ch PASCAL-EOF-CHAR))
          ((char=? pascal-ch #\})
            (read-pascal-ch)
            (skip-pascal-blanks))
          (else
            (read-pascal-ch)
            (skip-pascal-comment)))))

    (skip-pascal-comments
      (lambda ()
        (if (char=? pascal-ch #\{)
          (begin
            (read-pascal-ch)
            (skip-pascal-comment)
            (skip-pascal-comments)))))
 
    (add-ch-to-buffer-proper
      (lambda ()
        (if (< buffer-idx MAX-TOKEN)
          (begin
            (set! pascal-buffer (cons pascal-ch pascal-buffer))
            (set! buffer-idx (+ buffer-idx 1))))
        (read-pascal-ch)
        ))
            
    (add-ch-to-buffer
      (lambda ()
        (set! pascal-ch (char-upcase pascal-ch))
        (add-ch-to-buffer-proper)))

    (build-symbol-buffer
      (lambda ()
        (if (or (pascal-char-alpha-numeric? pascal-ch) (char=? pascal-ch #\_))
          (begin
            (add-ch-to-buffer)
            (build-symbol-buffer)))))

    (build-string-buffer
      (lambda ()
        (cond
          ((char=? pascal-ch PASCAL-EOF-CHAR))
          ((char=? pascal-ch #\')
            (read-pascal-ch)
            (if (char=? pascal-ch #\')
              (begin
                (add-ch-to-buffer)
                (build-string-buffer))))
          (else
            (add-ch-to-buffer-proper)
            (build-string-buffer)))))

    (add-negative-sign
      (lambda ()
        (if (char=? pascal-ch #\-)
          (add-ch-to-buffer))))

    (build-number-buffer
      (lambda ()
        (letrec (
          (state-2
            (lambda ()
              (add-ch-to-buffer)
              (cond
                ((char=? pascal-ch #\.)
                  (if (char=? (get-lookahead-pascal-ch) #\.)
                    (state-7)
                    (state-3)))
                ((char-numeric? pascal-ch) (state-2))
                ((or (char=? pascal-ch #\e) (char=? pascal-ch #\E)) (state-4))
                (else (state-7)))))
          (state-3
            (lambda ()
              (add-ch-to-buffer)
              (cond
                ((or (char=? pascal-ch #\e) (char=? pascal-ch #\E)) (state-4))
                ((char-numeric? pascal-ch) (state-3))
                (else (state-7)))))
          (state-4
            (lambda ()
              (add-ch-to-buffer)
              (cond
                ((or (char=? pascal-ch #\+) (char=? pascal-ch #\-)) (state-5))
                ((char-numeric? pascal-ch) (state-6))
                (else (state-7))))) ; TODO should be an error
          (state-5
            (lambda ()
              (add-ch-to-buffer)
              (if (char-numeric? pascal-ch)
                (state-6)
                (state-7)))) ; TODO should be an error
          (state-6
            (lambda ()
              (add-ch-to-buffer)
              (cond
                ((char-numeric? pascal-ch) (state-6))
                (else (state-7)))))
          (state-7
            (lambda () '()))
        )
        (if (char-numeric? pascal-ch)
          (state-2)))))

    (separator?
      (lambda ()
        (or (memq pascal-ch separators) (char=? pascal-ch #\-))))

    (build-separator-buffer
      (lambda ()
        (let
          ((old-ch pascal-ch))
          (add-ch-to-buffer)
          (if (member (list old-ch pascal-ch) two-character-separators)
            (add-ch-to-buffer)))))

    (create-number
      (lambda ()
        (set! token (buffer->number))))

    (create-symbol
      (lambda ()
        (set! token (buffer->symbol))))

    (create-string
      (lambda ()
        (set! token (buffer->string))
        (if (= (string-length token) 1) ; convert string to a character
          (set! token (string-ref token 0)))))

    )

    (set! pascal-buffer '())
 
    (skip-pascal-blanks)
    (if (char=? pascal-ch #\{)
      (skip-pascal-comments))
    (cond
      ((char=? pascal-ch PASCAL-EOF-CHAR)) ; token is ()
      ((separator?)
        (build-separator-buffer)
        (create-symbol))
      ((or (char-numeric? pascal-ch) (char=? pascal-ch #\-))
        (add-negative-sign)
        (build-number-buffer)
        (create-number))
      ((or (char-alphabetic? pascal-ch) (memq pascal-ch '(#\_ #\$)))
        (add-ch-to-buffer)
        (build-symbol-buffer)
        (create-symbol))
    ; ((char=? pascal-ch #\^) ; special case the pointer symbol
    ;   (add-ch-to-buffer)
    ;   (create-symbol))
      ((char=? pascal-ch #\')
        (read-pascal-ch)
        (build-string-buffer)
        (create-string))
      (else
        (pascal-error "bad character" pascal-ch)
        (add-ch-to-buffer)
        (create-symbol)))
;(my-write-pascal token) (my-newline-pascal)
;(write token) (newline)
    token))
