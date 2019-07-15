#lang racket

;this file includes-------------------
;[1] basic functions to operate on list
;[2]Predicates
;[3]Selectors
;[4]Constructors
;-------------------------------------
;[Selectors];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-op expn) (car expn))
(define (get-arg-lst  expn ) (cdr expn))
(define (get-arg1 expn) (cadr expn))
(define (get-function-kernal exp) (cadr exp))
(define (get-function-arg exp) (caddr exp))
(define (get-deriv-kernal exp) (cadr exp))
(define (get-deriv-arg exp) (caddr exp))
(define (get-eq-lhs exp ) (cadr exp))
(define (get-eq-rhs exp ) (caddr exp))

(define (base p) (cadr p))
(define (exponent p) (caddr p))
#|
> (get-op `(+ x y z))
+
> (get-arg-lst `(+ x y z))
(x y z)
> (get-arg1 `(+ x y z))
x
> (get-function-kernal `(function f x))
f
> (get-function-arg `(function f x))
x
> (get-deriv-arg `(deriv f x))
x
> (get-eq-lhs `(= y x))
y
> (get-eq-rhs `(= y x))
x
> (base `(** x n))
x
> (exponent `(** x n))
n|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;[Predicates];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (variable? v) (symbol? v))

(define (variable=? v1 v2)
  (and (variable? v1) (variable? v2) (equal? v1 v2)))

(define (symbol=? x y) (string=? (symbol->string x) (symbol->string y)))
(define (symbol<? x y) (string<? (symbol->string x) (symbol->string y)))
(define (symbol<=? x y) (string<=? (symbol->string x) (symbol->string y)))
(define (symbol>? x y) (string>? (symbol->string x) (symbol->string y)))
(define (symbol>=? x y) (string>=? (symbol->string x) (symbol->string y)))


(define (expression? x)
  (or (number? x) (variable? x) (pair? x)))
;and also number=? which is inbuilt

(define (expression=? e1 e2)
  (equal? e1 e2))

(define (expression<? e1 e2)
  (cond [(and (number? e1) (number? e2)) (< e1 e2)]
        [(number? e1) #t]
        [(number? e2) #f]
        [(and (symbol? e1) (symbol? e2)) (symbol<? e1 e2)]
        [(symbol? e1) #t]
        [(symbol? e2) #f]
        [(and (pair? e1) (pair? e2)) (cond [(expression<? (car e2) (car e1)) #f]
                                           [(expression=? (car e2) (car e1)) (cond [(null? (cdr e1)) #t]
                                                                                   [(null? (cdr e2)) #f]
                                                                                   [else (expression<? (cdr e1) (cdr e2))])]
                                           [else #t])]
        [else (error "Invalid Input")]))

(define (expression>? e1 e2)
  (expression<? e2 e1))

(define (function? exp) (and (pair? exp) (equal? 'function (get-op exp))))
(define (deriv? exp) (and (pair? exp) (equal? 'deriv (get-op exp))))

(define (abs? x) (and (pair? x) (eq? (get-op x) 'abs)))
(define (log? x) (and (pair? x) (eq? (get-op x) 'log)))
(define (sin? x) (and (pair? x) (eq? (get-op x) 'sin)))
(define (cos? x) (and (pair? x) (eq? (get-op x) 'cos)))
(define (tan? x) (and (pair? x) (eq? (get-op x) 'tan)))
(define (cosec? x) (and (pair? x) (eq? (get-op x) 'cosec)))
(define (sec? x) (and (pair? x) (eq? (get-op x) 'sec)))
(define (cot? x) (and (pair? x) (eq? (get-op x) 'cot)))
(define (asin? x) (and (pair? x) (eq? (get-op x) 'asin)))
(define (acos? x) (and (pair? x) (eq? (get-op x) 'acos)))
(define (atan? x) (and (pair? x) (eq? (get-op x) 'atan)))
(define (acosec? x) (and (pair? x) (eq? (get-op x) 'acosec)))
(define (asec? x) (and (pair? x) (eq? (get-op x) 'asec)))
(define (acot? x) (and (pair? x) (eq? (get-op x) 'acot)))

(define (sqrt? x) (and (pair? x) (eq? (get-op x) 'sqrt)))
(define (eqn? exp) (and (pair? exp) (equal? (get-op exp) '=)))
(define (sum? exp) (and (pair? exp) (equal? (get-op exp) '+)))
(define (product? exp) (and (pair? exp) (equal? (get-op exp) '*)))
(define (exponentiation? exp) (and (pair? exp) (equal? (get-op exp) '**)))
#|
;> (variable? 1)
;#f
;> (expression? 1)
;#t
;> (expression? 'x)
;#t
;> (expression? `(+  x t (+ c r)))
;#t
;> (sort `(1 2 x t (+ c t)) expression<?)
;(1 2 t x (+ c t))
;> (sort `(1 2 x (+ c t (* x r)) (+ c t)) expression<?)
;(1 2 x (+ c t) (+ c t (* x r)))
;>
> (sum? `(+ x t))
#t
> (product? `(* x t))
#t
> (exponentiation? `(* x t))
#f
> (exponentiation? `(** x t))
#t|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[Constructors];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-function f x) (list 'function f x))
(define (make-deriv exp var) (list 'deriv exp var))
(define (make-abs x) (if (number? x) (abs x) (list 'abs x)))
(define (make-log x) (if (number? x) (log x) (list 'log x)))
(define (make-sin x) (if (number? x) (sin x) (list 'sin x)))
(define (make-cos x) (if (number? x) (cos x) (list 'cos x)))
(define (make-tan x) (if (number? x) (tan x) (list 'tan x)))
(define (make-cosec x) (if (number? x) (/ 1 (sin x)) (list 'cosec x)))
(define (make-sec x) (if (number? x) (/ 1 (cos x)) (list 'sec x)))
(define (make-cot x) (if (number? x) (/ 1 (tan x)) (list 'cot x)))
(define (make-asin x) (if (number? x) (asin x) (list 'asin x)))
(define (make-acos x) (if (number? x) (acos x) (list 'acos x)))
(define (make-atan x) (if (number? x) (atan x) (list 'atan x)))
(define (make-acosec x) (list 'acosec x))
(define (make-asec x) (list 'asec x))
(define (make-acot x) (list 'acot x))
(define (make-eqn lhs rhs) (list '= lhs rhs))
(define (make-sqrt x) (if (number? x) (sqrt x) (list 'sqrt x)))

;> (make-sin 'x)
;(sin x)
;> (make-eqn 'x `(* y t))
;(= x (* y t))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;[basic functions];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (function-chain f-lst);function chain
  (cond [(null? (cdr f-lst)) (car f-lst)]
        [else (lambda (x) ((car f-lst) ((function-chain (cdr f-lst)) x)))]))

;slice
(define (slice l i j)
  (define (slice-helper l c)
    (cond [(< c i) (slice-helper (cdr l) (+ c 1))]
          [(<= c j) (cons (car l) (slice-helper (cdr l) (+ c 1)))]
          [else `()]))
  (slice-helper l 1))

;and-list
(define (and-list lst)
  (foldr (lambda (x y) (and x y)) #t lst))

;delete-pos
(define (delete-pos pos lst) 
  (cond [(null? lst) (error "Index out of range")]
        [(= 0 pos) (cdr lst)]
        [else (cons (car lst) (delete-pos (- pos 1) (cdr lst)))]))

;list-intersect
(define (list-intersect lsts);lsts is a list of list
  (set->list (apply set-intersect (map list->set lsts))))

;list-union
(define (list-union lsts)
  (set->list (apply set-union (map list->set lsts))))

;is-present
(define (is-present? e l)
  (cond [(null? l) #f]
        [(equal? (car l) e) #t]
        [else (is-present? e (cdr l))]))

;pack
(define (pack l)
  (cond [(null? l) l]
        [else (let*([res (pack (cdr l))])
                (cond[(null? res) (list (cons (car l) 1))]
                     [(equal? (car l) (caar res)) (cons (cons (car l) (+ 1 (cdar res))) (cdr res))]
                     [else (cons (cons (car l) 1) res)]))]))

;independent?
(define (independent? exp var)
  (cond [(number? exp) #t]
        [(variable? exp) (not (variable=? var exp))]
        [(null? exp) #t]
        [(equal? (car exp) var) #f]
        [else  (andmap (lambda (se) (independent? se var)) exp)]))
#|
 > (independent? 1 'x)
#t
> (independent? 'c 'x)
#t
> (independent? 'x 'x)
#f
> (independent? `(+ z (+ x  c)) 'x)
#f
|#

;insert
(define (insert-at i e l)
  (append (take l i) (list e) (drop l i)))

;is-member
(define (is-member? l1 l)
  (subset? (list->set l1) (list->set l)))

;remove-members
(define (remove-members l1 l)
  (define (remove-members-helper l ans)
    (cond [(null? l) ans]
          [(is-present? (car l) l1) (remove-members-helper (cdr l) ans)]
          [else (remove-members-helper (cdr l) (cons (car l) ans))]))
  (reverse (remove-members-helper l `())))

;get-index
(define (get-index e l)
  (define (get-index-helper l ans)
    (cond [(null? l) #f]
          [(and (list? (car l)) (is-present? e (car l))) ans]
          [(equal? e (car l)) ans]
          [else (get-index-helper (cdr l) (+ 1 ans))]))
  (get-index-helper l 0))

;cprod
(define (cprod l)
  (cond [(null? l) `(())]
        [else (append*(map (lambda (x) (map (lambda (l1) (cons x l1)) (cprod (cdr l))))
                           (car l)))]))

(define (list-mixed-up l a)
  (cond[(null? l) '()]
       [(null? (cdr l)) l]
       [else (cons (car l) (cons a (list-mixed-up (cdr l) a)))])
  )

;label-freq
(define (label-freq l)
  (define table (make-hash))
  (define (label-helper l)
    (cond [(null? l) table]
          [(hash-has-key? table (car l)) (begin (hash-set! table (car l) (+ 1 (hash-ref table (car l))))
                                                (label-helper (cdr l)))]
          [else (begin (hash-set! table (car l) 1)
                       (label-helper (cdr l)))]))
  (label-helper l))
;> (label-freq `(a a c f s a  c))
;#hash((f . 1) (c . 2) (s . 1) (a . 3))

;;;;
(define (exp-replace-helper exp to-replace replace)
  (define (exp-replace-helper1 exp)
    (cond [(not (list? exp)) exp]
          [(null? exp) `()]
          [(equal? (car exp) to-replace) (cons  replace (exp-replace-helper1 (cdr exp)))]
          [else (cons (exp-replace-helper1 (car exp))
                      (exp-replace-helper1 (cdr exp)))]))
  (exp-replace-helper1 exp))
(define (exp-replace exp to-replace-lst replace-lst)
  (cond [(null? to-replace-lst) exp]
        [else (exp-replace (exp-replace-helper exp (car to-replace-lst) (car replace-lst))
                           (cdr to-replace-lst)
                           (cdr replace-lst))]))
;> (define exp `(+ x (** a x) z))
;> (exp-replace exp `(x z) `(x1 z1))
;(+ x1 (** a x1) z1)


(define (make-exponentiation x n)
  (cond [(equal? n 0) 1]
        [(equal? n 1) x]
        [(and (number? x) (number? n)) (expt x n)]
        [(or (symbol? x) (number? x)) (list '** x n)]
        [(exponentiation? x) (make-exponentiation (base x) (make-product (list n (exponent x))))]
        [(product? x) (make-product (map (lambda (y) (make-exponentiation y n)) (get-arg-lst x)))]
        [else (list '** x n)]))

;(make-exponentiation '(* x y z) 'n) ;'(* (** x n) (** y n) (** z n))

(define (gather-data-type pred? op-for-type identity sequence)
  (define (gather-helper seq rem-seq gath)
    (cond [(null? seq) (list rem-seq gath)]
          [(pred? (car seq)) (gather-helper (cdr seq) rem-seq (op-for-type (car seq) gath))]
          [else (gather-helper (cdr seq) (cons (car seq) rem-seq)  gath)]))
  (let*([res (gather-helper sequence `() identity)]
        [ans (sort (cons (cadr res) (car res)) expression<?)])
    ans))
;(gather-num + 0 (list 1 'a 2 'd 3 'c 'b)) ;'(6 a b c d)
;
;[hof]
(define (merge-same-op is-op? arg)
  (cond [(null? arg) arg]
        [(is-op? (car arg)) (append (merge-same-op is-op? (cdar arg)) (merge-same-op is-op? (cdr arg)))]
        [else (cons (car arg) (merge-same-op is-op? (cdr arg)))]))

;> (merge-same-op sum? `(1 x (+ x z)))
;(1 x x z)
;> (merge-same-op product? `(1 x (* x z)))
;(1 x x z)
;
;[hof]
(define (make-op op-func op-symb unit-num args)
  (let*([g-n (gather-data-type number? op-func unit-num args)])
    (cond [(null? (cdr g-n)) (car g-n)]
          [(and (equal? unit-num (car g-n))
                (null? (cddr g-n))) (cadr g-n)]
          [(equal? unit-num (car g-n)) (cons op-symb (cdr g-n))]
          [else (cons op-symb g-n)])))

(define (make-sum1 args) (make-op + '+ 0 (merge-same-op sum? args)))
(define (make-product1 args)
  (let*([ans (make-op * '* 1 (merge-same-op product? args))])
    (cond [(number? ans) ans]
          [(variable? ans) ans]
          [(is-present? 0 ans) 0]
          [else ans])))
;(merge-same-op sum? '(1 2 (+ 3 4) (* 5 6))) ;'(1 2 3 4 (* 5 6))
;(make-sum1 '(a (+ 1 c) b 3 (* 2 b))) ;'(+ 4 a b c (* 2 b))
;(make-product '(1 a (* 2 f e) b 4 c (+ 4 d))) ;'(* 8 a b c e f (+ 4 d))
;(make-product (list '(+ a b c))) ;'(+ a b c)

(define (simplify-additive exp)
  (define table (make-hash))
  (define (sa-helper args)
    
    (define (init-table l)
      (cond[(null? l) 'done]
           [(product? (car l)) (let*([c (get-arg1 (car l))]
                                     [ls (cdr (get-arg-lst (car l)))]
                                     [lst (if (null? (cdr ls)) (car ls) (cons '* ls))])
                                 (cond [(hash-has-key? table lst) (let*([val (hash-ref table lst)]
                                                                        [new-val (make-sum1 (list c val))])
                                                                    (begin (hash-set! table lst new-val)
                                                                           (init-table (cdr l))))]
                                       [else (begin (hash-set! table lst c)
                                                    (init-table (cdr l)))]))]
           [else (cond [(hash-has-key? table (car l)) (let*([val (hash-ref table (car l))]
                                                            [new-val (make-sum1 (list 1 val))])
                                                        (begin (hash-set! table (car l) new-val)
                                                               (init-table (cdr l))))]
                       [else (begin (hash-set! table (car l) 1)
                                    (init-table (cdr l)))])]))
    (begin (init-table args)
           (map (lambda (l1) (let*([key (car l1)]
                                   [cd (and (list? key) (not (null? (cdr key))))]
                                   [value (cdr l1)])
                               (cond[(equal? value 1) key]
                                    [cd (make-product1 (list value key))]
                                    [else (make-product1 (list value key))])))
                (hash->list table))))
  
  (let*([rexp  exp])
    (cond [(sum? rexp) (let*([res1 (sa-helper (get-arg-lst exp))]
                             [res (make-sum1 res1) ])
                         res)]
          [else rexp])))

;> (simplify-additive (make-sum1 `(a (+ 1 c) b 3 (* 2 b))))
;(+ 4 a c (* 3 b))

(define (simplify-multiplicative exp)
  (define (sm-helper args)
    (define table (make-hash))
    (define (init-table l)
      (cond[(null? l) 'done]
           [(exponentiation? (car l))(let*([b (base (car l))]
                                           [e (exponent (car l))])
                                       (cond [(hash-has-key? table b) (let*([val (hash-ref table b)]
                                                                            [new-val (make-sum1 (list e val))])
                                                                        (begin (hash-set! table b new-val)
                                                                               (init-table (cdr l))))]
                                             [else (begin (hash-set! table b e)
                                                          (init-table (cdr l)))]))]
           [else (cond [(hash-has-key? table (car l)) (let*([val (hash-ref table (car l))]
                                                            [new-val (make-sum1 (list 1 val))])
                                                        (begin (hash-set! table (car l) new-val)
                                                               (init-table (cdr l))))]
                       [else (begin (hash-set! table (car l) 1)
                                    (init-table (cdr l)))])]))
    (begin (init-table args)
           (map (lambda (l1) (let*([key (car l1)]
                                   [value (cdr l1)])
                               (make-exponentiation key value)))
                (hash->list table))))
  (let*([rexp exp])
    (cond [(product? rexp)(let*([res1 (sm-helper (get-arg-lst exp))]
                                [res (make-product1 res1)])
                            res)]
          [else rexp])))
;(simplify-multiplicative (make-product1 `((+ 1 c) b 3 ( ** (+ 1 c) -1) (* 2 b))))
;(* 6 (** b 2))

;until p f x

(define (until p f x)
  (if (p x) x
      (until p f (f x))))

(define (make-sum2 args) (until (lambda (x) (equal? x (simplify-additive x))) simplify-additive (make-sum1 args)))
;> (make-sum2 `(x x (* 2 x) 2  (+ x x)))
;(+ 2 (* 6 x))
;done in 3 steps m-s1-(simplify-additive)>make-sum
(define (make-product2 args) (until (lambda (x) (equal? x (simplify-multiplicative x))) simplify-multiplicative (make-product1 args)))

;> (make-product2 `(x x (* x x) (** x -4)))
;1


;reconstruct-helper again constructs the exp in decent manner
(define (reconstruct-helper exp)
  (cond [(number? exp) exp]
        [(variable? exp) exp]
        [(function? exp) exp]
        [(deriv? exp) exp]
        [(eqn? exp) (make-eqn (reconstruct-helper(get-eq-lhs exp)) (reconstruct-helper (get-eq-rhs exp)))]
        [(abs? exp) (make-abs (reconstruct-helper (get-arg1 exp)))]
        [(log? exp) (make-log (reconstruct-helper (get-arg1 exp)))]
        [(sin? exp) (make-sin (reconstruct-helper (get-arg1 exp)))]
        [(cos? exp) (make-cos (reconstruct-helper (get-arg1 exp)))]
        [(tan? exp) (make-tan (reconstruct-helper (get-arg1 exp)))]
        [(cosec? exp) (make-cosec (reconstruct-helper (get-arg1 exp)))]
        [(sec? exp) (make-sec (reconstruct-helper (get-arg1 exp)))]
        [(cot? exp) (make-cot (reconstruct-helper (get-arg1 exp)))]
        [(asin? exp) (make-asin (reconstruct-helper (get-arg1 exp)))]
        [(acos? exp) (make-acos (reconstruct-helper (get-arg1 exp)))]
        [(atan? exp) (make-atan (reconstruct-helper (get-arg1 exp)))]
        [(acosec? exp) (make-acosec (reconstruct-helper (get-arg1 exp)))]
        [(asec? exp) (make-asec (reconstruct-helper (get-arg1 exp)))]
        [(acot? exp) (make-acot (reconstruct-helper (get-arg1 exp)))]
        [(exponentiation? exp) (make-exponentiation (reconstruct-helper (base exp)) (reconstruct-helper (exponent exp)))]
        [(sum? exp) (make-sum2 (map reconstruct-helper (get-arg-lst exp)))]
        [(product? exp) (make-product2 (map reconstruct-helper (get-arg-lst exp)))]))
(define (make-sum args)
  (until (lambda (x) (equal? x (reconstruct-helper x))) reconstruct-helper (make-sum2 args)))
(define (make-product args)
  (until (lambda (x) (equal? x (reconstruct-helper x))) reconstruct-helper (make-product2 args)))

;same as make-sum is done in 2 steps.
;(sa-helper `(x y x (* -1 x) (* -1 y)))
;#hash((y . 0) (x . 1))(0 x)

;> (sa-helper `(x y x (* -5 x) (* -1 y)))
;#hash((y . 0) (x . -3))(0 (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* -2 y)))
;#hash((y . -1) (x . -3))((* -1 y) (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* x y)))
;#hash((y . (+ 1 x)) (x . -3))((* y (+ 1 x)) (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* x y) z))
;#hash((z . 1) (y . (+ 1 x)) (x . -3))(z (* y (+ 1 x)) (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* x y) z (* z 2)))
;#hash((z . 1) (2 . z) (y . (+ 1 x)) (x . -3))(z (* 2 z) (* y (+ 1 x)) (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* x y) z (* (+ a b) z)))
;#hash((z . (+ 1 a b)) (y . (+ 1 x)) (x . -3))((* z (+ 1 a b)) (* y (+ 1 x)) (* -3 x))

;> (sa-helper `(x y x (* -5 x) (* x y) z (* (+ a b) z)))
;#hash((z . (+ 1 a b)) (y . (+ 1 x)) (x . -3))((* z (+ 1 a b)) (* y (+ 1 x)) (* -3 x))

;> (simplify-multiplicative `(* x (** x -1)))
;1

;> (define exp `(* x x (** x 3) (** x -2) (** y a) (** y 1) z (** z -1)))
;> exp
;(* x x (** x 3) (** x -2) (** y a) (** y 1) z (** z -1))
;> (simplify-multiplicative exp)
;(* (** x 3) (** y (+ 1 a)))
;> (define exp `(* x x (** x 3) (** x -2) (** y a) (** y 1) z (** z -1) a b c))
;> (simplify-multiplicative exp)
;(* a b c (** x 3) (** y (+ 1 a)))
;> (define exp `(* x x (** x 3) (** x -2) (** y 2) (** y -1) (** y -1) z (** z -1) a b c))
;> (simplify-multiplicative exp)
;(* a b c (** x 3))
;> (define a (make-product `(x (+ 2 x) (+ 2 x) (* 2 x) (** (* 2 x) a))))
;> a
;(* (** 2 (+ 1 a)) (** x (+ 2 a)) (** (+ 2 x) 2))

;;;;;;[simplification methods];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify exp)
  (define (rewrite exp)
    (cond [(number? exp) exp]
          [(variable? exp) exp]
          [(function? exp) exp]
          [(deriv? exp) exp]
          [(eqn? exp) (make-eqn (rewrite(get-eq-lhs exp)) (rewrite (get-eq-rhs exp)))]
          [(abs? exp) (make-abs (rewrite (get-arg1 exp)))]
          [(log? exp) (make-log (rewrite (get-arg1 exp)))]
          [(sin? exp) (make-sin (rewrite (get-arg1 exp)))]
          [(cos? exp) (make-cos (rewrite (get-arg1 exp)))]
          [(tan? exp) (make-tan (rewrite (get-arg1 exp)))]
          [(cosec? exp) (make-cosec (rewrite (get-arg1 exp)))]
          [(sec? exp) (make-sec (rewrite (get-arg1 exp)))]
          [(cot? exp) (make-cot (rewrite (get-arg1 exp)))]
          [(asin? exp) (make-asin (rewrite (get-arg1 exp)))]
          [(acos? exp) (make-acos (rewrite (get-arg1 exp)))]
          [(atan? exp) (make-atan (rewrite (get-arg1 exp)))]
          [(acosec? exp) (make-acosec (rewrite (get-arg1 exp)))]
          [(asec? exp) (make-asec (rewrite (get-arg1 exp)))]
          [(acot? exp) (make-acot (rewrite (get-arg1 exp)))]
          [(exponentiation? exp) (make-exponentiation (rewrite (base exp)) (rewrite (exponent exp)))]
          [(sum? exp) (make-sum (map rewrite (get-arg-lst exp)))]
          [(product? exp) (make-product (map rewrite (get-arg-lst exp)))]))
  (rewrite exp))
;hof which takes a pattern matching predicate(corresponding to an op)
;and replaces all the sub-exp(that satisfies predicate) by alternative-exp\

(define (pattern-matcher-and-replacer exp is-my-Pattern? pat-len replacement);return type of replacement should be appropriate
  (define (pattern-matcher-and-replacer-helper exp res)
    (cond [(and (or (variable? exp) (number? exp))
                (is-my-Pattern? exp)) replacement]
          [else (cond [(> pat-len (length exp)) (reverse (append exp res))]
                      [else (let*([l1 (take exp pat-len)]
                                  [rem-exp (drop exp pat-len)])
                              (cond [(is-my-Pattern? l1) (pattern-matcher-and-replacer-helper rem-exp (append replacement res))]
                                    [else (pattern-matcher-and-replacer-helper (cdr exp) (cons (car exp) res))]))])]))
  (pattern-matcher-and-replacer-helper exp `()))
(define (sin2x? exp)
  (match exp
    [`((* 2 (cos x) (sin x))) #t]
    [_ #f]))

(define (make-pattern-simplifier P pat-len replacement)
  (lambda (exp) (pattern-matcher-and-replacer exp P pat-len replacement)))

(define sin2x-simplifier;
  (make-pattern-simplifier sin2x? 1 `((sin (* 2 x)))))







(provide (all-defined-out))






