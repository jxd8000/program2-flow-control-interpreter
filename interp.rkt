#lang racket

;;;; ***************************************************
;;;; Jianhao Deng
;;;; Darion Achilles Gomez
;;;; Chloe de Lamare
;;;; CSDS 345 Spring 2026
;;;; Project 1
;;;; ***************************************************

(require "state.rkt")
(require "eval.rkt")
(require "simpleParser.rkt")

(provide interpret M_statementlist-cps M_statement)

;interpret
(define interpret
  (lambda (filename)
    (let* ((ast (parser filename))
           (final-state (M_statementlist-cps ast (empty-state))))
      (state-get-return final-state))))

; abstractions for M_statementlist
(define empty? null?)
(define statementof car)
(define statementlistof cdr)
(define statement-statementlist? pair?)


; M_statementlist-cps
; statementlist state -> state
(define M_statementlist-cps
  (lambda (statementlist state next return break continue throw)
    (cond
      ((empty? statementlist) (return state)) ; If the statementlist is empty, then no changes to state occur
      (else (M_statementlist-cps (statementlistof statementlist)
                             (M_statement (statementof statementlist) state) return break continue throw)))))

; abstractions for M_statement
(define keyword car)
(define name-of cadr)
(define name-of-assignment cadr)
(define expression-of-statement caddr)
(define expression cadr)

; M_statement-cps
; statement state -> state
(define M_statement
  (lambda (statement state next return break continue throw)
    (cond
      ((eq? (keyword statement) 'var) (if (= (length statement) 3) ; if 'var is present we know its a declare statement, additionally if '= is present we must consider expression
                                           (M_declare2 (name-of statement) (expression-of-statement statement) state) ; use diff declare to handle expression
                                           (M_declare1 (name-of statement) state)))
      ((eq? (keyword statement) 'return) (M_return (expression statement) state)) ;return - change to return continuation?
      ((eq? (keyword statement) 'if) (M_if statement state))
      ((eq? (keyword statement) 'while) (M_while statement state))
      ((eq? (keyword statement) '=) (M_assign (name-of-assignment statement) (expression-of-statement statement) state))
      (else (error 'badop "Invalid statement form: ~s" statement)))))

;M_declare1 - declares a variable
; returns a new state for the new variable declared
(define M_declare1
  (lambda (name state next return break continue throw)
    (cond
      ((state-has? name state) (error 'badop "Redefining variable: ~s" name)) ; if it already exists
      (else (state-declare name state)))))

;M_declare2 - declares a variable while with a value
; returns a new state for the new variable declared
(define M_declare2
  (lambda (name expression state next return break continue throw)
    (cond
      ((state-has? name state) (error 'badop "Redefining variable: ~s" name))
      (else (state-declare/init name (M_expression expression state) state)))))

(define M_assign
  (lambda (name expression state next return break continue throw)
    (interpret "sample_programs/t15prof.txt")(state-update name (M_expression expression state) state)))

(define M_return ; instead of using state-set-return use return continuation
  (lambda (expression state state next return break continue throw)
    (state-set-return
     (convert-bool (M_expression expression state)) state)))

(define convert-bool
  (lambda (v)
    (if (boolean? v)
        (if v
            'true
            'false)
        v)))

(define M_if
  (lambda (statement state next return break continue throw)
    (let* ([cond-expr (cadr statement)]
           [then-stmt (caddr statement)]
           [has-else? (>= (length statement) 4)])
      (if (M_boolean cond-expr state)
          (M_statement then-stmt state)
          (if has-else?
              (M_statement (cadddr statement) state)
              state)))))

;(define cond-expr cadr statement
; ask teammates for consistent coding style for less confusion

(define M_while
  (lambda (statement state next return break continue throw)
    (let ([cond-expr (cadr statement)]
          [body-stmt (caddr statement)])
      (let loop ([st state])
        (if (M_boolean cond-expr st)
            (loop (M_statement body-stmt st))
            st)))))

    
    
      
; continuation is used to jump in the code, next jump to next line of code, for each statement type where do I need to jump to
; where is this code supposed to do
; where in the interpreter is the final output done?