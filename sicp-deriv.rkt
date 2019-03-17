#lang racket
;;section 2.3.2 - symbolic differentiation

;;reduction rules
;dc/dx = 0
;dx/dx = 1
;d(u+x)/dx = du/dx + dv/dx
;d(uv) = u(dv/dx) + v(du/dx)

;;make a variable a symbol
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2) (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

;list that has a + as the first char
(define (sum? e) (and (pair? e) (eq? (car e) '+)))

(define (make-sum% a1 a2) (list '+ a1 a2))

;;is the expression a number and are the equal
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;now try and reduce to simpliest form
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list '+ a1 a2))))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e)
  (second e))

;(define (multiplicand e)
;  (third e))

(define (addend e)
  (second e))
;(define (augend e)
;  (third e))

;;2 or more thing - not 1 or more things - start with the secon and keep printing things
;'(+ x y (+ x 3) 4)
;(augend '(+ a b c d e f)); =>'(+ b (+ c (+ d (+ e f))))
(define (augend s)
  (if (<= (length s) 3);has 2 things then do what we always do show the second arg, like make it look like a pair
      (third s)
      (list '+ (third s) (augend (cdr s))))) ;otherwise it's getting it into the same format that worked earlier
   
(define (multiplicand e)
  (if (<= (length e) 3)
      (third e)
      (list '* (third e) (multiplicand (cdr e))))); get a say it's (* a b (+ 1 2) 4) => (* b (* (+ 1 2) 4)); which is the augend in the format that woked before

(define (make-product% m1 m2)
  (list '* m1 m2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) ;some simplifications
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list '* m1 m2)))) ;what we actually want
;to add exponent rule we need ot see if it's an exponennt
(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e)
  (second e))
(define (exponent e)
  (third e))

(define (make-exponentiation b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        ((and (number? b) (number? n)) (expt b n)); simplicifcatoins
        (else
         (list '** b n)))) ;the real representation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- deriv " exp))))


;;test it out
(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; now goes to 'y before it would be like 0 x + 1 y
(deriv '(* (* x y) (+ x 3)) 'x); '(+ (* x y) (* (+ x 3) y))

;;do the rule d(u^n)/dx = nu^(n-1) (du/dx)

;;2.58a - (x + (3 * (x + (y + 2))))
(define (addend-2.58a s) (car s))
;(define (augend-2.58a s) (third s)); since it always takes 2

(define (augend-2.58a s)
  (if (<= (length s) 3);has 2 things then do what we always do
      (third s)
      (list (third s) '+ (augend (cdr s))))) ;otherwise it's getting it into the same format that worked earlier
   
(define (multiplicand-2.58a e)
  (if (<= (length e) 3)
      (third e)
      (list (third e) '* (multiplicand (cdr e)))));

(define (make-sum-2.58a a1 a2)
  (cond ((=number? 0 a1) a2)
        ((=number? 0 a2) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (multiplier-2.58a p) (car p))

(define (make-product-2.58a a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (=number? a1) (number? a2)) (* a1 a2))
        (else
         (list multiplier '* multiplicand-2.58a))))

(define (product?-2.58a e)
  (and (pair? e) (eq? (second e) '*)))

(define (sum?-2.58a e)
  (and (pair? e) (eq? (second e) '+)))

(define (deriv-2.58a exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-2.58a exp)
         (make-sum-2.58a (deriv-2.58a (addend-2.58a exp) var)
                   (deriv-2.58a (augend-2.58a exp) var)))
        ((product?-2.58a exp)
         (make-sum-2.58a (make-product-2.58a (multiplier-2.58a exp) (deriv-2.58a (multiplicand-2.58a exp) var))
                         (make-product-2.58a (multiplicand-2.58a exp) (deriv-2.58a (multiplier-2.58a exp) var))))
        (else
         (error "unknown expression type -- deriv " exp))))

(deriv-2.58a '(x + (3 * (x + (y + 2)))) 'x)
(deriv-2.58a '(x + (3 * x) + (2 * x)) 'x)



        
         

  



