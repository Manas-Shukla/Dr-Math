#lang racket
(require "fundamental.rkt")
(require "show-expr.rkt")
;vec? is different from vector?
;
;;[ASSUMPTION ::ARG IS IN DOMAIN OF FUNCTION]

;vectors;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vec? v) (and (list? v) (not (null? v)))) 

(define (null-vec? v) (and (vec? v) (equal? (make-list (length v) 0) v)))


(define (scale-vec v x)
  (map (lambda (v1) (make-product (list x v1))) v))

; (scale-vec `(a c (+ x e 3 (** x 2) (** x 2))) 2)
;output=((* 2 a) (* 2 c) (* 2 (+ 3 e x (* 2 (** x 2)))))

(define (vec-add . v)
  (define null-vec (make-list (length (car v)) 0))
  (foldr (lambda (v1 v2) (map (lambda (x y) (make-sum (list x y))) v1 v2)) null-vec v))

; (vec-add `(x y z) `((* -1 x) (* 2 z) z))
;output=(0 (+ y (* 2 z)) (* 2 z))  

(define (linear-combination lcoeff . v)
  (define id (make-list (length (car v)) 0))
  (foldl vec-add id (map (lambda (l1 l2) (scale-vec l2 l1)) lcoeff v)))

;(linear-combination `(2 0 9 8) `(1 0 0) `(0 1 0) `(0 0 1) `(x y z))
;output=((+ 2 (* 8 x)) (* 8 y) (+ 9 (* 8 z)))


(define (vec-dot-prod v1 v2)
  (make-sum (map (lambda (c1 c2) (make-product (list c1 c2))) v1 v2)))

;(vec-dot-prod `(z x c) `((** z -1) (** z -1) (* 2 x (** c -1))))
;(+ 1 (* 2 x) (* x (** z -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;matrices (represented as list of rows);row index starts from 0 and same for col
(define (matrix? m) (and (list? m) (not(null? m)) (andmap vec? m)));pending
(define (square-mat? m) (and (matrix? m) (equal? (length m) (length (car m))))) ;assuming m is a matrix(for below func as welll)
(define (null-mat? m) (and (matrix? m) (andmap null-vec? m)))


(define (identity-mat? m)
  (equal? m (make-identity-mat (length m))))

(define (make-matrix . l);matrix constructor
  l)

(define (make-identity-mat n)
  (define (r i) (append (make-list i 0) (list 1) (make-list (- n i 1) 0)))
  (define (make-iden-mat-helper i)
    (cond [(= i n) `()]
          [else (cons (r i) (make-iden-mat-helper (+ i 1)))]))
  (make-iden-mat-helper 0))
;(make-identity-mat 4)
;((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))

(define (print-mat m);matrix displayer
  (cond [(null? m) (display "")]
        [else (begin (displayln (car m))
                     (print-mat (cdr m)))]))
;basic selectors (self-explanatory)
(define (num-rows m) (length m))

(define (num-cols m) (length (car m)))

(define (get-row i m) (list-ref m i))

(define (order-mat m) (cons  (num-rows m) (num-cols m)))

(define (get-col j m) (map (lambda (l1) (list-ref l1 j)) m))

(define (mat-ref  m i j) (list-ref (get-row i m) j))

(define (mat-insert-col j col-vec m)
  (transpose-mat (insert-at j col-vec (transpose-mat m))))

(define (mat-insert-row i row-vec m)
  (insert-at i row-vec m))

(define (mat-delete-row i m)
  (delete-pos i m))

(define (mat-delete-col j m)
  (transpose-mat (delete-pos j (transpose-mat m))))

(define (mat-set-row i m row-vec)
  (append (take m i) (list row-vec) (drop m (+ i 1))))

(define (mat-set-col j m col-vec)
  (let*([mt (transpose-mat m)])
    (transpose-mat (append (take mt j) (list col-vec) (drop mt (+ j 1))))))

(define (mat-exchange-row i j m)
  (let*([ri (get-row i m)]
        [rj (get-row j m)]
        [res1 (mat-set-row i m rj)])
    (mat-set-row j res1 ri)))

(define (mat-exchange-col i j m)
  (let*([ci (get-col i m)]
        [cj (get-col j m)]
        [res1 (mat-set-col i m cj)])
    (mat-set-col j res1 ci)))



(define (get-diag m)
  (define (get-diag-helper k m)
    (cond [(null? m) `()]
          [else (cons (list-ref (car m) k) (get-diag-helper (+ k 1) (cdr m)))]))
  (get-diag-helper 0 m))
;(get-diag(make-identity-mat 4))
;(1 1 1 1)

(define (mat-trace m)
  (make-sum (get-diag m)))
;(mat-trace `((a b c) ((* 2 a) (* 2 3) (* c 0)) ((* -3 a) b (** c 2))))
;output=(+ 6 a (** c 2))

(define (scale-mat m x)
  (map (lambda (v1) (scale-vec v1 x)) m))
;scales every vec in m

(define (transpose-mat m)
  (define c (num-cols m))
  (define (transpose-helper j)
    (cond [(= j c) `()]
          [else (cons (get-col j m) (transpose-helper (+ j 1)))]))
  (transpose-helper 0))
;(transpose-mat `((x y z) (a b c) (f g h)))
;((x a f) (y b g) (z c h))

(define (mat-add . m);assuming they can be added
  (define null-matrix (make-list (num-rows (car m)) (make-list (num-cols (car m)) 0)))
  (foldr (lambda (m1 m2) (map vec-add m1 m2)) null-matrix m))
;(mat-add `((a b c)(1 2 3)) `((a b c)(-1 -2 e)) `((0 1 2)(a b c)))
;(((* 2 a) (+ 1 (* 2 b)) (+ 2 (* 2 c)))
;    (a       b           (+ 3 c e)))

(define (mat-prod-helper m1 m2);assuming they can be multiplied
  (define tm2 (transpose-mat m2))
  (define (mat-prod-helper1 m)
    (cond [(null? m) `()]
          [else (cons (map (lambda (v1) (vec-dot-prod (car m) v1)) tm2) (mat-prod-helper1 (cdr m)))]))
  (mat-prod-helper1 m1))
(define (mat-prod . m)
  (define (op m1 m2) (if (equal? 'end m2) m1
                         (mat-prod-helper m1 m2)))
  (foldr op `end m))
; (define M (mat-prod (make-identity-mat 4) `((a) (b) (c) (d)) `((a b c d))))
;M =
;(((** a 2) (* a b) (* a c) (* a d))
; ((* a b) (** b 2) (* b c) (* b d))
; ((* a c) (* b c) (** c 2) (* c d))
; ((* a d) (* b d) (* c d) (** d 2)))
;

(define (minor-mat i j m)
  (define (min-row l) (append (take l j) (drop l (+ j 1))))
  (define (minor-helper r m)
    (cond [(null? m) m]
          [(= r i) (minor-helper (+ r 1) (cdr m))]
          [else (cons (min-row (car m)) (minor-helper (+ r 1) (cdr m)))]))
  (minor-helper 0 m))
;(print-mat (minor-mat 1 2 M))
;((** a 2) (* a b) (* a d))
;((* a c) (* b c) (* c d))
;((* a d) (* b d) (** d 2))

(define (get-cofactors r m)
  (define col (num-cols m))
  (define (get-cof-helper c)
    (cond [(= c col) `()]
          [else (cons (list (expt -1 (+ r c)) (minor-mat r c m)) (get-cof-helper (+ c 1)))]))
  (get-cof-helper 0))
;(get-cofactors 0 M)
;((1 (((** b 2) (* b c) (* b d)) ((* b c) (** c 2) (* c d)) ((* b d) (* c d) (** d 2))))
; (-1 (((* a b) (* b c) (* b d)) ((* a c) (** c 2) (* c d)) ((* a d) (* c d) (** d 2))))
; (1 (((* a b) (** b 2) (* b d)) ((* a c) (* b c) (* c d)) ((* a d) (* b d) (** d 2))))
; (-1 (((* a b) (** b 2) (* b c)) ((* a c) (* b c) (** c 2)) ((* a d) (* b d) (* c d)))))

;1 and -1 are just tags of matrix which tell us what factor is used in calculating det

(define (mat-determinant m)
  (cond [(identity-mat? m) 1]
        [(and (null? (cdr m)) (null? (cdar m))) (caar m)]
        [else (make-sum (map (lambda (x y) (make-product (list  x (car y) (mat-determinant (cadr y)))))
                             (car m) (get-cofactors 0 m)))]))

(define m '((0 1 1) (0 a b) (g h i)))
;(mat-determinant m) ;(+ (* -1 a g) (* b g))
(show-expression (mat-determinant `((0 h g)(h 0 f)(g h 0))))
;(+ (* f g h) (* g (** h 2)))

(define (adjoint-mat m)
  (define (adjoint-mat-helper r l)
    (cond [(< r 0) l]
          [else (let*([res (map (lambda (y) (make-product (list  (car y) (mat-determinant (cadr y)))))
                                (get-cofactors r m))])
                  (adjoint-mat-helper (- r 1) (cons res l)))]))
  (cond [(and (null? (cdr m)) (null? (cdar m))) m]
        [else (transpose-mat (adjoint-mat-helper (- (num-rows m) 1) `()))]))
;(adjoint-mat m)
;((d (* -1 b)) ((* -1 c) a))

(define (inverse-mat m)
  (let*([det (mat-determinant m)]
        [adj-m (adjoint-mat m)])
    (cond [(equal? det 0) (error "inverse does not exist")]
          [else (scale-mat adj-m (make-exponentiation det -1))])))
;(inverse-mat `((1 2 3)(2 3 1)(3 1 2)))
;((-5/18 1/18 7/18) (1/18 7/18 -5/18) (7/18 -5/18 1/18))

(define (solve-the-system A b lvar);A is square mat 
  (define delta (mat-determinant A))
  
  (define delta-lst (map (lambda (j) (mat-determinant (mat-set-col j A b))) (range 0 (length A))))
  (define sol-lst (map (lambda (d1) (make-product (list d1 (make-exponentiation delta -1)))) delta-lst))
  (map (lambda (var ans) (make-eqn var ans)) lvar sol-lst))
;(solve-the-system `((1 2 3) (0 1 0) (2 2 0)) `(0 0 1) `(x y z))
;((= x 1/2) (= y 0) (= z -1/6))

(define (forward-eliminator M EP)
  (define row-exchange-counter 0)
  (define len (length M))
  (define len1 (length (car M)))
  (define m (list->vector M))
  (define ep (list->vector EP))
  (define (apply-row-transformation c cref rcref eprcref);change
    (cond [(= c (- len 1)) `done]
          [else (let*([rc1 (vector-ref m (+ c 1))]
                      [factor (make-product (list -1 (list-ref rc1 cref) (make-exponentiation (list-ref rcref cref) -1)))]
                      [new-rc1 (linear-combination (list factor 1) rcref rc1)]
                      [eprc1 (vector-ref ep (+ c 1))]
                      [new-eprc1 (linear-combination (list factor 1) eprcref eprc1)])
                  (begin (vector-set! m (+ c 1) new-rc1)
                         (vector-set! ep (+ c 1) new-eprc1)
                         (apply-row-transformation (+ c 1) cref rcref eprcref)))]))
  (define (get-first-non-zero-row-index c cref)
    (let*([rc (if (= c len) #f (vector-ref m c))])
      (cond [(= c len) #f]
            [(equal? (list-ref rc cref) 0) (get-first-non-zero-row-index (+ c 1) cref)]
            [else c])))
  (define (apply-row-exchange c)
    (let*([rc (vector-ref m c)]
          [cnew (get-first-non-zero-row-index c c)]
          [c1 (if (equal? cnew #f) c (begin (set! row-exchange-counter (+ 1 row-exchange-counter))
                                            cnew))]
          [rc1 (vector-ref m c1)]
          [eprc (vector-ref ep c)]
          [eprc1 (vector-ref ep c1)])
      (begin (vector-set! m c rc1)
             (vector-set! m c1 rc)
             (vector-set! ep c eprc1)
             (vector-set! ep c1 eprc))))
  (define (forwd-elim-helper count);count starts from 0
    (cond [(or (= count len1)(= count len)) 'done]
          [(equal? (list-ref (vector-ref m count) count) 0) (begin (apply-row-exchange count)
                                                                   
                                                                   (if (equal? (list-ref (vector-ref m count) count) 0)
                                                                       'done
                                                                       (forwd-elim-helper count)))]
          [else (begin (apply-row-transformation count count (vector-ref m count) (vector-ref ep count))
                       (forwd-elim-helper (+ 1 count)))]))
  (begin (forwd-elim-helper 0)
         (list row-exchange-counter (vector->list m) (vector->list ep))))
;(define M`((1 2 3) (0 1 0) (2 2 0)))
;(define (foo M) (reverse (map reverse M)))
;> (foo (cadr (forward-eliminator (foo (cadr (forward-eliminator M `((0)(0)(0))))) `((0)(0)(0)))))
;((1 0 0) (0 1 0) (0 0 -6))
(define (mat-determinant-by-gaussian-algorithm m)
  (let*([res (forward-eliminator m (make-list (length m) (list 0)))]
        [sign (expt -1 (car res))])
    (make-product (cons sign (get-diag (cadr res))))))







(provide (all-defined-out))
