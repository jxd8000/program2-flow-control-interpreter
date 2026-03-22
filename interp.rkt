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
    (let ((ast (parser filename)))
      (M_statementlist-cps
       ast
       (empty-state)
       (lambda (st) (error "Program ended without return"))
       (lambda (val st) val)
       (lambda (st) (error "break used outside loop"))
       (lambda (st) (error "continue used outside loop"))
       (lambda (val st) (error (format "Uncaught excepetion: ~a" val)))))))
          
           

;abstractions for M_blockofcode
(define beginningof car)
(define bodyof cdr)

; M_blockofcode
(define M_blockofcode
  (lambda (block state next return break continue throw)
    (let ((inner-state (push-layer state)))
      (M_statementlist-cps
       (bodyof block)
       inner-state
       (lambda (finished-state)
         (next (pop-layer finished-state)))
       (lambda (val return-state)
         (return val (pop-layer return-state)))
       (lambda (break-state)
         (break (pop-layer break-state)))
       (lambda (continue-state)
         (continue (pop-layer continue-state)))
       (lambda (val throw-state)
         (throw val (pop-layer throw-state)))))))

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
      ((empty? statementlist) (next state)) ; If the statementlist is empty, then no changes to state occur
      (else (M_statement (statementof statementlist) state
                         (lambda (new-state)
                           (M_statementlist-cps
                            (statementlistof statementlist) new-state next return break continue throw))
                         return break continue throw)))))

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
      ((eq? (keyword statement) 'begin) (M_blockofcode statement state next return break continue throw))
      ((eq? (keyword statement) 'var) (if (= (length statement) 3) ; if 'var is present we know its a declare statement, additionally if '= is present we must consider expression
                                          (M_declare2 (name-of statement) (expression-of-statement statement)
                                                      state next return break continue throw) ; use diff declare to handle expression
                                          (M_declare1 (name-of statement)
                                                      state next return break continue throw)))
      ((eq? (keyword statement) 'return) (M_return (expression statement) state next return break continue throw)) ;return - change to return continuation?
      ((eq? (keyword statement) '=) (M_assign (name-of-assignment statement)
                                              (expression-of-statement statement) state next return break continue throw))
      ((eq? (keyword statement) 'if) (M_if statement state next return break continue throw))
      ((eq? (keyword statement) 'while) (M_while statement state next return break continue throw))
      ((eq? (keyword statement) 'break) (M_break state next return break continue throw))
      ((eq? (keyword statement) 'continue) (M_continue state next return break continue throw))
      (else (error 'badop "Invalid statement form: ~s" statement)))))


;M_declare1 - declares a variable
; returns a new state for the new variable declared
(define M_declare1
  (lambda (name state next return break continue throw)
    (next (state-declare name state))))

;M_declare2 - declares a variable while with a value
; returns a new state for the new variable declared
(define M_declare2
  (lambda (name expression state next return break continue throw)
    (next (state-declare/init name (M_expression expression state) state))))

(define M_assign
  (lambda (name expression state next return break continue throw)
    (next (state-update name (M_expression expression state) state))))

(define M_return ; instead of using state-set-return use return continuation
  (lambda (expression state next return break continue throw)
    (return (convert-bool (M_expression expression state)) state)))

(define convert-bool
  (lambda (v)
    (if (boolean? v)
        (if v 'true 'false)
        v)))

(define M_if
  (lambda (statement state next return break continue throw)
    (let* ([cond-expr (cadr statement)]
           [then-stmt (caddr statement)]
           [has-else? (>= (length statement) 4)])
      (if (M_boolean cond-expr state)
          (M_statement then-stmt state next return break continue throw)
          (if has-else?
              (M_statement (cadddr statement) state next return break continue throw)
              (next state))))))

;(define cond-expr cadr statement
; ask teammates for consistent coding style for less confusion

(define M_while
  (lambda (statement state next return break continue throw)
    (let ([cond-expr (cadr statement)]
          [body-stmt (caddr statement)])
      (let loop ([st state])
        (if (M_boolean cond-expr st)
            (M_statement body-stmt
                         st (lambda (new-state)
                              (loop new-state))
                         return (lambda (break-state)
                                  (next break-state))
                         (lambda (continue-state)
                           (loop continue-state))
                         throw)
                         (next st))))))


(define M_break
  (lambda (state next return break continue throw)
    (break state)))

(define M_continue
  (lambda (state next return break continue throw)
    (continue state)))

(define M_throw
  (lambda (statement state next return break continue throw)
    (throw (M_expression (cadr statement) state) state)))

;; <try> ::= try { <statementlist> } catch ( <var> ) { <statementlist> } finally { <statementlist> } |
;;           try { <statement list > } catch ( <var> ) { <statementlist> } |
;;           try { <statementlist> } finally { <statementlist> }
;; (try body (catch (e) body) (finally body))
;; (try body (catch (e) body))
;; (try body (finally body))

(define M_try
  (lambda (statement state next return break continue throw)
    

    
; continuation is used to jump in the code, next jump to next line of code, for each statement type where do I need to jump to
; where is this code supposed to do
; where in the interpreter is the final output done?