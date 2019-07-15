#lang racket
;solves a equation in which the unknown appears only once
;eqn has '= as its outtermost-level operator
(require "fundamental.rkt")


(define (eqsolver eq var)
  (define eqn (simplify eq))
  (cond [(independent? eqn var) (error "the equation does not contain the given variable")]
        [else (let*([lhs (get-eq-lhs eqn)]
                    [rhs (get-eq-rhs eqn)])
                (cond [(independent? lhs var) (eqsolver-helper rhs lhs var)]
                      [else (eqsolver-helper lhs rhs var)]))]))
(define (eqsolver-helper dep-part indep-part var)
  (define (helper dep indep)
    (cond [(variable=? dep var) (make-eqn dep indep)]
          [else  (let*([op (get-op dep)]
                       [arg (get-arg-lst dep)]
                       [argd (if (null? (cdr arg)) (car arg) arg)])
                   (match op
                     ['log (helper argd (make-exponentiation (exp 1) indep))]
                     ['sin (helper argd (make-asin indep))]
                     ['cos (helper argd (make-acos indep))]
                     ['tan (helper argd (make-atan indep))]
                     ['asin (helper argd (make-sin indep))]
                     ['acos (helper argd (make-cos indep))]
                     ['atan (helper argd (make-tan indep))]
                     ['cosec (helper argd (make-acosec indep))]
                     ['sec (helper argd (make-asec indep))]
                     ['cot (helper argd (make-acot indep))]
                     ['acosec (helper argd (make-cosec indep))]
                     ['asec (helper argd (make-sec indep))]
                     ['acot (helper argd (make-cot indep))]
                     ['+ (let*([dse (car (filter (lambda (se) (not (independent? se var))) arg))]
                               [indse (make-sum (remove dse arg))])
                           (helper dse (make-sum (list indep (make-product (list -1 indse))))))]

                     ['* (let*([dse (car (filter (lambda (se) (not (independent? se var))) arg))]
                               [indse (make-product (remove dse arg))])
                           (helper dse (make-product (list indep (make-exponentiation indse -1)))))]
                     ['** (let*([b (base dep)]
                                [e (exponent dep)])
                            (cond [(independent? e var) (helper b (make-exponentiation indep (make-exponentiation e -1)))]
                                  [(independent? b var) (helper e
                                                                (make-product (list (make-log indep)
                                                                                    (make-exponentiation (make-log b) -1))))]))]
                     ))]))
  (helper dep-part indep-part))
;(eqsolver '(= y x) 'y) ;'(= y x)
;(eqsolver '(= x y) 'y) ;'(= y x)
;(eqsolver '(= (+ y z) x) 'y) ;'(= y (+ x (* -1 z)))
;(eqsolver '(= x (* z y)) 'y) ;'(= y (* x (** z -1)))
;(eqsolver '(= (** x z) y) 'x) ;'(= x (** y (** z -1)))
;(eqsolver '(= (** x z) y) 'z) ;'(= z (* (log y) (** (log x) -1)))
;(eqsolver '(= x (log y)) 'y) ;'(= y (** 2.718281828459045 x))

(provide (all-defined-out))