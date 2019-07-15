#lang racket
(require "fundamental.rkt")
(require "show-expr.rkt")
;consits of two functions
;1](deriv exp var)
;2](integ exp var);basic level
;deriv;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deriv exp var);can be extended by providing more base cases
  (define expr (simplify exp))
  (define (deriv-helper exp)
    (cond [(eqn? exp) (make-eqn (deriv-helper (get-eq-lhs exp) ) (deriv-helper (get-eq-rhs exp)))]
          [(number? exp) 0]
          ;independence-cd
          [(variable? exp) (if (variable=? var exp) 1
                               0)]
          [else (let*([op (get-op exp)]
                      [arg (get-arg-lst exp)]
                      [argd (if (null? (cdr arg)) (car arg) arg)])
                  (cond [(equal? op 'sin) (make-product (list (make-cos (car arg))
                                                              (deriv-helper argd )))]
                        [(equal? op 'cos) (make-product (list -1 (make-sin (car arg))
                                                              (deriv-helper argd )))]
                        [(equal? op 'tan) (make-product (list  (make-exponentiation (make-sec (car arg)) 2)
                                                               (deriv-helper argd )))]
                        [(equal? op 'cosec) (make-product (list -1 (make-cosec (car arg)) (make-cot (car arg))
                                                                (deriv-helper argd )))]
                        [(equal? op 'sec) (make-product (list (make-sec (car arg)) (make-tan (car arg))
                                                              (deriv-helper argd )))]
                        [(equal? op 'cot) (make-product (list -1 (make-exponentiation (make-cosec (car arg)) 2)
                                                              (deriv-helper argd)))]
                        [(equal? op 'log) (make-product (list (make-exponentiation (car arg) -1)
                                                              (deriv-helper argd )))]
                        [(equal? op '**) (let*([b (base exp)]
                                               [e (exponent exp)])
                                           (cond [(independent? e var) (make-product (list e
                                                                                           (make-exponentiation b
                                                                                                                (make-sum (list e -1)))
                                                                                           (deriv-helper b )))]
                                                 [(independent? b var) (make-product (list (make-exponentiation b e)
                                                                                           (make-log b)
                                                                                           (deriv-helper e )))]
                                                 [else (make-sum (list (make-product (list e
                                                                                           (make-exponentiation b
                                                                                                                (make-sum (list e -1)))
                                                                                           (deriv-helper b)))
                                                                       (make-product (list (make-exponentiation b e)
                                                                                           (make-log b)
                                                                                           (deriv-helper e)))))]))]
                        [(sum? exp) (make-sum (map (lambda (e) (deriv-helper e )) arg))]
                        [(product? exp)(let*([term1 (make-product (list (car arg)
                                                                        (deriv-helper (make-product (cdr arg)) )))]
                                             [term2 (make-product (list (deriv-helper (car arg) )
                                                                        (make-product (cdr arg))))])
                                         (make-sum (list term1 term2)))]))]))
  (deriv-helper expr))
;> (deriv `(** x x) `x)
;(+ (* (** x x) (log x)) (** x x))
;> (deriv `(+ x (+ 2 x) (cos x) (log (cos x))) `x)
;(+ 2 (* -1 (** (cos x) -1) (sin x)) (* -1 (sin x)))
;> (deriv `(* (log x) x) `x)
;(+ 1 (log x))
;> (deriv `(* x c (+ x x)) `x)
;(* 4 c x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (integrate exp var)
  (cond [(eqn? exp) (make-eqn (integrate (get-eq-lhs exp) var) (integrate (get-eq-rhs exp) var))]
        [(variable=? exp var) (make-product (list (/ 1 2) (make-exponentiation exp 2)))]
        [(independent? exp var) (make-product (list var exp))]
        [(and (equal? (list var) (cdr exp)) (sin? exp)) (make-product (list -1 (make-cos var)))]
        [(and (equal? (list var) (cdr exp)) (cos? exp)) (make-sin var)]
        [(product? exp) (let*([indep (make-product (filter (lambda (l1) (independent? l1 var)) (cdr exp)))]
                              [dep (make-product (filter (lambda (l1) (not (independent? l1 var))) exp))]
                              )
                          (make-product (list  indep (integrate dep var))))]
        [else (let*([op (get-op exp)]
                    [arg (get-arg-lst exp)]
                    [argd (if (null? (cdr arg)) (car arg) arg)])
                (cond [(equal? op '**) (let*([b (base exp)]
                                             [e (exponent exp)])
                                         (cond [(variable=? b var)
                                                (cond [(independent? e var)
                                                       (if (equal? e -1) `(log var)
                                                           (make-product (list (make-exponentiation var (make-sum (list e 1)))
                                                                                (make-exponentiation (make-sum (list e 1)) -1))))])]
                                               [(and (independent? b var)
                                                     (variable=? e var)) (make-product (list (make-log b)
                                                                                             (make-exponentiation b var)))]
                                               [else (error "unknown expression")]))]
                      [(equal? op '+) (make-sum (map (lambda (x) (integrate x var)) arg))]))]))
 (show-expression (integrate `(+ x (sin x) (cos x) (** x 3)) 'x))
;(+ (* -1 (cos x)) (* 1/4 (** x 4)) (* 1/2 (** x 2)) (sin x))
(provide (all-defined-out))                    























