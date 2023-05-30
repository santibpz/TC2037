#lang racket

; Deterministic Finite Automata that validates a mathematical expression.
; Name: Santiago Benitez
; ID: A01782813

(require racket/trace)

(provide arithmetic-lexer)

(struct dfa (func initial-state accept-states)) ; structure for the dfa object


(define (evaluate-dfa dfa-to-eval str) ; dfa evaluation function
    (let loop ([chars (string->list str)]
             [state (dfa-initial-state dfa-to-eval)] ; state 
             [tokens '()] ; tokens list
             [value '()]) ; value list
    (if (empty? chars)
        (if (member state (dfa-accept-states dfa-to-eval)) ; if state is valid
            (if (eq? state 'spa) ; if final state is 'spa ignore in output
                (reverse tokens) ; result 
                (reverse (cons (list (list->string (reverse value)) state) tokens))
                )
            #f) ; if expression is wrong, return invalid
        (let-values ([(new-state token-found) ((dfa-func dfa-to-eval) state (car chars))])
          (loop (cdr chars)
                new-state ; new state
                (if token-found ; if token is found create list with token and value found
                      (cons (list (list->string (reverse value)) token-found) tokens)                                
                      tokens)
                (if (not (char-whitespace? (car chars))) ; ignore whitespace in value
                    (if token-found
                    (cons (car chars) '()) 
                    (cons (car chars) value))
                    (if (eq? state 'comment) ; it the state is a comment, whitespace is not ignored
                        (cons (car chars) value)
                        '())))))))


; transition function to determine if an input is a math expression

(define (transition-fn state char)
  (case state
    ['start (cond
              [(or (eq? char #\-)(eq? char #\+)) (values 'sign #f)]
              [(char-numeric? char) (values 'int #f)]
              [(or (char-alphabetic? char) (eq? char #\_)) (values 'var #f)]
              [(eq? char #\() (values 'par_open #f)]
              [(char-whitespace? char) (values 'init_spa #f)]
              [else (values 'inv #f)])]
    ['init_spa (cond 
                [(eq? char #\() (values 'par_open #f)] 
                [(or (char-alphabetic? char) (eq? char #\_)) (values 'var #f)]
                [(char-numeric? char) (values 'int #f)]
                [(char-whitespace? char) (values 'init_spa #f)]
                [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)] 
                [else (values 'inv #f)]
                )]
    ['sign (cond
             [(char-numeric? char) (values 'int #f)]
             [else (values 'inv #f)])]
    ['int (cond
            [(char-numeric? char) (values 'int #f)]
            [(eq? char #\.) (values 'dot #f)]
            [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
            [(char-whitespace? char) (values 'spa 'int)]
            [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op 'int)]
            [(eq? char #\)) (values 'par_close 'int)]
            [else (values 'inv #f)])]
    ['dot (cond
            [(char-numeric? char) (values 'float #f)]
            [else (values 'inv #f)])]
    ['float (cond
              [(char-numeric? char) (values 'float #f)]
              [(or (eq? char #\e) (eq? char #\E)) (values 'e #f)]
              [(char-whitespace? char) (values 'spa 'float)]
              [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op 'float)]
              [(eq? char #\)) (values 'par_close 'float)]
              [else (values 'inv #f)])]
    ['e (cond
          [(char-numeric? char) (values 'exp #f)]
          [(or (eq? char #\-)(eq? char #\+)) (values 'e_sign #f)]
          [else (values 'inv #f)])]
    ['exp (cond
            [(char-numeric? char) (values 'exp #f)]
            [(char-whitespace? char) (values 'spa 'exp)]
            [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op 'exp)]
            [(eq? char #\)) (values 'par_close 'exp)]
            [else (values 'inv #f)])]
    ['e_sign (cond
               [(char-numeric? char) (values 'exp #f)]
               [else (values 'inv #f)])]
    ['spa (cond
            [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op #f)]
            [(eq? char #\)) (values 'par_close #f)]
            [(char-whitespace? char) (values 'spa #f)]
            [else (values 'inv #f)])]
    ['op (cond
           [(char-whitespace? char) (values 'op_spa 'op)]
           [(or (char-alphabetic? char) (eq? char #\_)) (values 'var 'op)]
           [(char-numeric? char) (values 'int 'op)]
           [(or (eq? char #\+) (eq? char #\-)) (values 'sign 'op)]
           [(eq? char #\() (values 'par_open 'op)]
           [(eq? char #\/) (values 'comment #f)] ; added comment
           [else (values 'inv #f)])]
    ['op_spa (cond
               [(or (char-alphabetic? char) (eq? char #\_)) (values 'var #f)]
               [(char-numeric? char) (values 'int #f)]
               [(eq? char #\() (values 'par_open #f)]
               [(char-whitespace? char) (values 'op_spa #f)]
               [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)] ; consider negatives after op_spa
               [else (values 'inv #f)])]
    ['comment (cond
               [(char-alphabetic? char) (values 'comment #f)]
               [(char-whitespace? char) (values 'comment #f)])] ; comment
    ['var (cond
            [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op 'var)]
            [(or (char-alphabetic? char) (eq? char #\_) (char-numeric? char)) (values 'var #f)]
            [(char-whitespace? char) (values 'spa 'var)]
            [(eq? char #\)) (values 'par_close 'var)]
            [else (values 'inv #f)]
            )]
    ['par_open (cond
                 [(char-numeric? char) (values 'int 'par_open)]
                 [(char-whitespace? char) (values 'parenthesis_spa 'par_open)]
                 [(or (char-alphabetic? char) (eq? char #\_)) (values 'var 'par_open)]
                 [(or (eq? char #\+) (eq? char #\-)) (values 'sign 'par_open)]
                 [else (values 'inv #f)])]
    ['parenthesis_spa (cond
                        [(char-numeric? char) (values 'int #f)]
                        [(or (char-alphabetic? char) (eq? char #\_)) (values 'var #f)]
                        [(or (eq? char #\+) (eq? char #\-)) (values 'sign #f)]
                        [(char-whitespace? char) (values 'parenthesis_spa #f)]
                        [else (values 'inv #f)]                       
                        )]
    ['par_close (cond
                  [(or (eq? char #\+) (eq? char #\-) (eq? char #\*) (eq? char #\/) (eq? char #\=) (eq? char #\^)) (values 'op 'par_close)]
                  [(char-whitespace? char) (values 'spa 'par_close)]
                  [else (values 'inv #f)]
                  )]
    [else (values 'inv #f)]))



; dfa instance to be used
(define dfa-instance (dfa transition-fn 'start '(int float exp var spa par_close comment)))

; arithmetic lexer definition
(define (arithmetic-lexer str)
  (evaluate-dfa dfa-instance str))

