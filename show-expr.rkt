#lang racket
(require "fundamental.rkt"
         2htdp/image)

(define (show-expression exp)
  (define (show x) (text x 24 "black"))
  (define blank (text " " 12 "black"))
  (define show+ (show "+"))
  (define show* blank)
  (define (add-parentheses x) (beside/align "bottom" (show "(") x (show ")")))
  (define (besidee lst)
    (let([len (length lst)])
      (cond[(= len 0) (show "1")]
           [(= len 1) (car lst)]
           [else (apply beside/align (cons "bottom" lst))])))
  (cond [(number? exp) (show (number->string exp))]
        [(variable? exp) (show (symbol->string exp))]
        [(eqn? exp) (beside/align "bottom" (show-expression (get-eq-lhs exp))
                                  blank
                                  (show "=")
                                  blank
                                  (show-expression (get-eq-rhs exp)))]
        [(sum? exp) (let ([res (besidee (list-mixed-up
                                         (map show-expression (get-arg-lst exp)) ;here get-arg-list gives cdr exp
                                         show+))])
                      (add-parentheses res)
                      )]
        [(log? exp) (beside/align "bottom" (show "log")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(exponentiation? exp) (beside/align "bottom" (show-expression (base exp))
                                             (above (show-expression (exponent exp))
                                                    blank))]
        [(sin? exp) (beside/align "bottom" (show "sin")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(cos? exp) (beside/align "bottom" (show "cos")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(tan? exp) (beside/align "bottom" (show "tan")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(cosec? exp) (beside/align "bottom" (show "cosec")
                                    (add-parentheses (show-expression (get-arg1 exp))))]
        [(sec? exp) (beside/align "bottom" (show "sec")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(cot? exp) (beside/align "bottom" (show "cot")
                                  (add-parentheses (show-expression (get-arg1 exp))))]
        [(acot? exp) (beside/align "bottom" (show "acot")
                                   (add-parentheses (show-expression (get-arg1 exp))))]
        [(atan? exp) (beside/align "bottom" (show "atan")
                                   (add-parentheses (show-expression (get-arg1 exp))))]
        [(asin? exp) (beside/align "bottom" (show "asin")
                                   (add-parentheses (show-expression (get-arg1 exp))))]
        [(acosec? exp) (beside/align "bottom" (show "acosec")
                                     (add-parentheses (show-expression (get-arg1 exp))))]
        [(asec? exp) (beside/align "bottom" (show "asec")
                                   (add-parentheses (show-expression (get-arg1 exp))))]
        [(acos? exp) (beside/align "bottom" (show "acos")
                                   (add-parentheses (show-expression (get-arg1 exp))))]
        [(product? exp) (define (denominator? x)
                          (and (exponentiation? x)
                               (number? (exponent x)) (< (exponent x) 0)))
                        (let([numerator (filter (function-chain (list not denominator?))
                                                (get-arg-lst exp))]
                             [denominator (map (lambda (x) (make-exponentiation (base x) (- (exponent x))))
                                               (filter denominator? (get-arg-lst exp)))]
                             )
                          (let ([draw-numerator (besidee (list-mixed-up
                                                          (map (lambda (x) (show-expression x)) numerator)
                                                          show*))]
                                [draw-denominator (besidee (list-mixed-up
                                                            (map (lambda (x) (show-expression x)) denominator)
                                                            show*))])
                            (if (null? denominator)
                                draw-numerator
                                (above
                                 draw-numerator
                                 (rectangle (max (image-width draw-numerator)
                                                 (image-width draw-denominator)) 2 "solid" "black")
                                 draw-denominator))
                            
                            )
                          )
                        
                        ]
        ))
(provide (all-defined-out))