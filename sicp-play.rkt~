#lang racket
(define (test a)
  (print a))


(define (sq x)
  (* x x))


(define (fib n)
  (if (or (= n 1) (= n 0))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fast-exp b n)
  (cond ((= 0 n)
        1)
        ((even? n) (sq (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))



(define (fib-iter n)
  (fib-iter-h n 0 1))
(define (fib-iter-h n a b)
  (if (< n 0)
      a
      (fib-iter-h (- n 1) b (+ a b))))




(define (fast-exp-i b n)
  (fast-exp-iter b n 1))

(define (fast-exp-iter b n acc)
  (cond ((= 0 n) acc)
        ((even? n) (fast-exp-iter (sq b) (/ n 2) acc))
        ((odd? n) (fast-exp-iter b (- n 1) (* acc b)))))

;(fast-exp-iter 2 7 1)

(define (print-expts fn b n count)
  (cond ((> count 0)
         (print n)
         (print " ")
         (print (fn b n))
         (newline)
         (print-expts fn b (+ n 1) (- count 1)))
        (else null)))


;;1.3 Formulating abstractions with higher-order procedures

;;think about the cube abstraction - it's param is a number - but what if we wanted to work with cubes, we need a way for procedures to accept procedures as arguments and return procedures as results if we want to deal with "concepts"
;;
(define (cube x)
  (* x x x))

;;sums integers from a -> b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;;sum the cube of integers
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum-pi a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (sum-pi (+ a 4) b))))

      
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (identity x) x)

;(sum identity 1 inc 10)
;(sum cube 1 inc 10)
;(* 8.0 (sum (lambda (x) (/ 1 (* x (+ x 2)))) 1 (lambda (x) (+ x 4)) 1000))


;;so one way to do an integral
(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2)) (lambda (x) (+ x dx)) b)
     dx))

;;Exercise 1.29
;;simpson's rule: (h/3)*[yo + 4y1 + 2y2 + ... 2y(n-1) + 4y(n-1) + yn]
;; h = (b - a)/ n for an even n
;; yk = f(a + kh)
;;do this in steps with wishful thinking
;;n must be even
(define (integral-simpsons f a b n)
  (define h (/ (- b a) n)) ;this is very easy got h out of the way
  (define (inc x) (+ x 1)) ;; next function just increments n by 1
  (define (y k) (f (+ a (* k h)))); just write it out, y(k) = f(a + k*h), ie a function that is the same as f(a) but with the inputs changed -- this was given in the question
  (define (term k) ;;fancy trick we need to do something like 4 * y(k) or like 2*y(k) or 1 * y(k), so just have h(x) = 4 * g(x) || and have g(x) = f(a + x*h) ||everything is a constant
    (* (cond ((odd? k) 4)
             ((or (= k n) (= k 0)) 1)
             ((even? k) 2)
             (else null))              
       (y k)))
  (* (/ h 3)
     (sum term ;;(term a) and term wil have something like (* 4 (y a))
          0
          inc ;since we know we go from 0 to n ie we need y(0) + 4y(1) + 2y(2) ... + 4y(n-1) + y(n)
          n)))
;;(sum term a next n))) we know that n goes up by one each time (notice a -> n not a -> b | since that's the sum we're looking for )


(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))

;;and look how much faster it is
;(equal? (simpson cube 1 10 100.0)
;        (integral cube 1 10 .001))


;;1.30 - sum iterative
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
 
 
;;1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (* (term a) result))))
  (iter a 1))

;;formula for pi/4 = (2/3 * 4/3) * (4/5 * 6/5)
;;just group it like above
(define (pi-term-product n)
  (* 4.0
     (product (lambda (x)
                (* (/ (- x 1) x)
                   (/ (+ x 1) x)))
              3
              (lambda (x) (+ x 2))
              n)))

(define (accumulator combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulator combiner null-value term (next a) next b))))

(define (accum-product term a next b)
  (accumulator * 1 term a next b))

(define (pi-term-product-accum n)
  (* 4.0
     (accum-product
      (lambda (x)
        (* (/ (- x 1) x)
           (/ (+ x 1) x)))
      3
      (lambda (x) (+ x 2))
      n)))
;(= (pi-term-product 100) (pi-term-product-accum 100)); #t

(define (accum-sum term a next b)
  (accumulator + 0 term a next b))
(accum-sum identity 1 inc 100); 5050 as expected



;1.33
;;make a filter so it only accecpts certain terms
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))



(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (sq (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))



(define (filtered-accumulator combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (term (filter a))
                (filtered-accumulator combiner null-value term (next a) next b filter))))

;;use 1.33a - sum of squares of the prime numbers 
(define (sum-square-primes a b)
  (filtered-accumulator + 0 sq a inc b (lambda (x)
                                               (cond ((prime? x)
                                                      (newline)
                                                      (print x)
                                                      x)
                                                     (else 0)))))

;;1.33b - product of numbers less than n that are prime to it ie GCD(x, n) = 1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relative-primes n)
  (filtered-accumulator * 1 identity 1 inc n (lambda (x)
                                               (if (= 1 (gcd x n)) (begin (newline) (print x) x) 1)))) 
                                                   
                                                  
;;takes a method for if a guess is good enough
;;takes a method 
(define (iterative-improvement good-enough? improve-guess guess)
  (lambda (guess)
    (let ((next-guess (improve-guess guess)))
      (if (good-enough? guess next-guess)
          next-guess
          (iterative-improvement good-enough? improve-guess next-guess)))))


;;for understainding 1.45

(define tolerance .0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (let ((next-guess (f first-guess)))
    (if (close-enough? next-guess first-guess)
        next-guess
        (fixed-point f next-guess))))

(define (average-damping f) 
  (lambda (x) (average x (f x))))

(define (repeat f k)
  (define (repeat-helper f k acc)
    (if (<= k 1)
        acc
        (repeat-helper f (- k 1) (compose f acc))))  ;; compose the original function with the modified one
  (repeat-helper f k f))

(define (average a b)
  (/ (+ a b) 2))

(define (compose f g)
  (lambda (x)
    (f (g x))))


(define h (average-damping inc))

(define g ((lambda (x) (average-damping (average-damping x)))
           inc))

;(g 3); => 3.25
;(h 3); => 4.0

((average-damping h) 3.0); => 3.25
((lambda (x) (/ (+ x (h x)) 2)) 3.0); => 3.25

;;zero is a procedure that takes a function f and transforms it so that the function f' is just the identity function returns whatever arg was passed
(define zero
  (lambda (f)
    (lambda (x) x)))
((zero inc) 3); 

(define (zero-alternate f)
  (lambda (x) x))

;;add one takes a function n and returns a function that takes a function f and manipulates both 
(define (add1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))


;(((add1 zero) inc) 4) ;;add1 takes a function n and returns something that takes a function and applies it one time.
;;basically the whole point of everything give me a function f, and i will transform it to something else
;(((lambda (f) (lambda (x) (f ((zero f) x)))) inc) 4); => 5
;(((lambda (f) (lambda (x) (f ((lambda (x) x) x)))) inc) 4); =>5
;(((lambda (f) (lambda (x) (f x))) inc) 4); => 5
;;all of the above are equivalent

;;add1 + 0 = 1 so this must be one
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
;((one inc) 4); =>5 ;looks like it works


;;what about 2
;(((add1 one) inc) 4); => 6
;(((lambda (f)
;    (lambda (x) (f ((one f) x))))
;  inc)
; 4) =>6

;(one f) => (lambda (x) (f x)) ; the function 
;((one f) x) => ((lambda (x) (f x)) x) => (f x) ; the function applied to x is just (f x)

;;;note this looks like 2 applied to inc and then to an arg 4 means the function is called twice
;(((lambda (f)
;   (lambda (x) (f (f x))))
; inc)
; 4); => 6;

(define two
  (lambda (f)
    (lambda (x) (f (f x))))) ;=> 6

;((two inc)  4) ;=> 6

;(two f) => (lambda (f (f x)))
;((two f) x) =>   (f (f x))

;((lambda (f) (lambda (x)
;              ((two f) ((two f) x))))
; inc); => a function that does

;(((lambda (f) (lambda (x)
;                ((two f) ((two f) x))))
;  inc)
; 0); => 4

;(let ((a (lambda (x) (f (f x)))
;      (b (f (f x)))))
;  (a b)) => (f (f (f (f x)))); gets 4 of them right away like 2 + 2 = 4 calls

(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
;(m f) ==> a from above
;((n f) x) ==> b from above see how it adds up correclty

;(lambda (f) (f x)) or (lambda (f) (f (f (f x)))) KEY IS THAT a (in our example a function (one f) or (three f)) THE FIRST PART IS A FUNCTION THAT TAKES b
;;and b IS JUST THE !!!!RESULT!!! OF A FUNCTION --- IT'S JUST (f (f x)) LIKE ((three f) x)
;;the syntax of (m f) ((n f) x) allows us to do this compostion



;;Ex 2.2 print point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))


;;2.3 rectangles in a plain
;;pick a point and length + width
;;or pick 2 points that are at opposite ends
;;or just one line segment that is the diagonal
(define (make-rectangle point l w)
  (list point l w))

;(define (get-length rect)
;  (second rect))
;(define (get-width rect)
;  (third rect))
(define (get-area rect)
  (* (get-length rect)
     (get-width rect)))
(define (get-perimiter rect)
  (* 2 (+ (get-length rect)
          (get-width rect))))
;;this works above

;;just make this the second example as well
;;see how either way works
(define (make-rect2 p1 p2)
  (make-segment p1 p2))

(define (get-dim-helper seg f)
  (abs (- (f (start-segment seg))
          (f (end-segment seg)))))

(define (get-width seg)
  (get-dim-helper seg x-point))

(define (get-length seg)
  (get-dim-helper seg y-point))
              


;;exercise 2.7 intervals

(define (add-interval x y)
  (make-interval (+ (lower-bound x)(lower-bound y))
                 (+ (upper-bound x)(upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)(lower-bound y)))
        (p2 (* (lower-bound x)(upper-bound y)))
        (p3 (* (upper-bound x)(lower-bound y)))
        (p4 (* (upper-bound x)(upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

  (define (make-interval a b)
    (cons a b))

  (define (upper-bound interval)
    (cdr interval))
  (define (lower-bound interval)
    (car interval))



(define interval1 (make-interval 5 10))
(define interval2 (make-interval 10 20))

(define (sub-interval x y)
  (make-interval (- (lower-bound y) (upper-bound x))
                 (- (upper-bound y) (lower-bound x))))

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))


;;ex 2.9
;;show that these two are always equivalent
;(equal? (width (add-interval interval1 interval2))
;        (+ (width interval1) (width interval2))); => t on paper just write out each of these and prove it

(define (span-zero-interval? x)
  (if (and (< (lower-bound x) 0)
           (> (upper-bound x) 0))
      #f
      #t))
;;look at div and see that the y interval part of it gets weird by def since the (make-interval (/ 1 upperbound) (/ 1 lowerbound)) [+,-] so lower bound ends up being higher than upper bound and makes no sense

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (average (upper-bound i)(lower-bound i)))

(define (make-center-percent c p)
  (make-interval (* c (- 1.0 p)) (* c (+ 1.0 p))))
(define (get-percent i)
  (- 1.0 (/ (lower-bound i) (center i))))


(define (par1 r1 r2)
  (div-interval (mul-interval  r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define i1 (make-center-percent 100 .04))
(define i2 (make-center-percent 200 .03))
;(div-interval i1 i1)
;'(0.9230769230769231 . 1.0833333333333333)
;(div-interval i2 i2)
;'(0.9417475728155339 . 1.0618556701030928)
;(get-percent (div-interval i1 i1))
;0.07987220447284338
;(get-percent (div-interval i2 i2))
;0.05994604855629937

;;2.17 - last pair of nonempty list
(define (last-pair l)
  (define (last-pair-helper l elem)
    (if (null? l)
        elem
        (last-pair-helper (cdr l) (car l))))
  (last-pair-helper l (car l)))

;;2.18 - reverse
(define (reverse l)
  (define (reverse-h l1 l2)
    (if (null? l1)
        l2
        (reverse-h (cdr l1) (cons (car l1) l2))))
  (reverse-h l null))


;;2.19
(define us-coins '(25 50 5 10 1))
(define uk-coins '(100 50 25 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1) ;only time it counts as change
        ((or (< amount 0) (null? coin-values)) 0); doesn't count
        (else
         (+ (cc (- amount (car coin-values)) coin-values); 50 - 5 - 5 - 5...we need the full value of coin-values 
            (cc amount (cdr coin-values)))))); say it's 97 you can't use a looney to make that skip to the next coin

;2.20 - Same parity
(define (same-parity . l)
  (let ((parity-func even?))
    (if (odd? (car l))
        (set! parity-func odd?)
        (set! parity-func even?)) ;makes you add the else statment that can't be nil
    (define (sp-helper l acc)
      (if (null? l)
          acc
          (if (parity-func (car l))
              (sp-helper (cdr l) (append acc (list (car l))));(sp-helper (cdr l) (cons (car l) acc)); this way reverses the order
              (sp-helper (cdr l) acc))))
    (sp-helper l '())))


;;map over a list
(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))
;;abstract this away
(define (mymap f items)
  (if (null? items)
      null
      (cons (f (car items))
            (mymap f (cdr items)))))

;;now we can do this the same way
(define (scale-list2 items factor)
  (mymap (lambda (x) (* x factor)) items))

;;2.22
;;i did this same error earlier look up like 2 exercises
;;what he's doing is this main line
;(cons (f (car list-of-vals)))
;       acc)
;say we had a list with (v1 v2 v3) this is the result of our function
;(cons v3 (cons v2 (cons v1 nil)) => (v3 v2 v1)
;;the ohter way its' a (cons (f (car l)) acc)

;;now part two he switches it
;(cons acc (f (car l)))
;initial val of acc is nil
;(( (nil . v1) . v2) . v3); so the order is right but the structure is way off

(define (foreach proc lst)
  (if (null? lst)
      #t
      (or (proc (car lst)) (foreach proc (cdr lst)))))

;;do a count leaves, basically should give 4 instead of 3 '((1 2) 3 4)
(define (count-leaves l)
  (cond ((null? l) 0)
        ((not (pair? l)) 1)
        (else 
         (+ (count-leaves (car l))
            (count-leaves (cdr l))))))

;;;2.24
;(list 1 (list 2 (list 3 4 )));    (1 (2 (3 4)))
;;;2.25
;(car (cdaddr '(1 3 (5 7) 9)))
;(caar '((7)))


;;2.18 - deep reverse ((1 2) (3 4)) => ((4 3) (2 1))
(define (deep-reverse lst)
  (define (rec l acc)
    (cond ((null? l) acc)
          ((pair? (car l)) ;special case if we see a list
           (rec (cdr l) (cons (rec (car l) null) acc))); reverse the list and then add it to the accumulator 
          (else
           (rec (cdr l) (cons (car l) acc)))))
  (rec lst null))
;;simple recursive def
(define (deep-rev2 l)
  (cond ((null? l) null)
        ((pair? (car l))
         (append (deep-rev2 (cdr l)) (list (deep-rev2 (car l))))) ; (ie '((xxx) (yyy)) => '((yyy) (xxx)) ;we need to switch everything around; had to build this incrmentally 
        (else
         (append (deep-rev2 (cdr l))
                 (list (car l))))))

;;so this way works but is there a better way?
(define (fringe-1 lst)
  (let ((acc null))
    (define (rec l)
      (cond ((null? l) acc)
            ((not (pair? (car l)))
             (set! acc (append acc (list (car l))))
             (rec (cdr l)))
            (else
             (rec (car l))
             (rec (cdr l)))))
    (rec lst)
    acc))

;;note how it's the same pattern all the time
(define (fringe lst)
  (cond ((null? lst) null)
        ((not (pair? (car lst)))
         (append (list (car lst)) (fringe (cdr lst))))
        (else
         (append (fringe (car lst))
                 (fringe (cdr lst))))))

;;2.28
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (cond ((pair? (branch-structure branch))
         (total-weight (branch-structure branch))); if the branch-structure is a mobile we need to get the total weight of the mobile
        (else 
         (branch-structure branch)))) ;base case - the branch-struture is a weight we just need that number 

(define (total-weight mobile)
  (+ (branch-weight (right-branch mobile))
     (branch-weight (left-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (mobile-balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch)) ;if branch is a mobile
      (mobile-balanced? (branch-structure branch))
      #t))

(define (all-mobile-balanced? mobile)
  (and (mobile-balanced? mobile)
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))


(define mobile1 (make-mobile (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 300 5))) (make-branch 1 20))); notice

;;d if we want to change the def of make-branch make-mobile to use car, then we need to change right-branch and branch-structure to be cdr instead of cadr (ie just the second item in either case)


;;;;mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) null); cdr tree will be null eventually
        ((not (pair? tree)) (* tree factor)) ;when looking at a single element
        (else
         (cons (scale-tree (car tree) factor) ;make the tree again 
               (scale-tree (cdr tree) factor)))))

;(scale-tree '( 1 (2 (3 (4 (5 6))))) 2)

(define (scale-tree-map tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree-map subtree factor) ;map sees a list '(1 (a b) 2 4) and grabs each element and will accumulate it. so if it gets a '(a b) it knows what to do  
             (* factor subtree))); if it's an element it gets accumulated. 
       tree))
;note if we give it a list we get a list with map '(1 2 3 (a b)) => (1 2 3 (a b)) if we just did (map identity '(1 2 3 (a b)))
;when we see if an element like (a b) is a list we just need to map that value and return it


;2.30 - square tree
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) ;if element then square it
         (* tree tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

;much smaller as it takes the consing away from us, it helps us combine what we need
(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (* subtree subtree)))
       tree))
; (square-tree '( 1 (2 (3 (4 (4 5) 5) (5 6))))); 

;now just abstract it
(define (tree-map tree func)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map subtree func)
             (func subtree)))
       tree))

;(tree-map '(1 2 ( 2 3 (4 (5) 6) (5 6) 7)) sq) => works
(define (square-tree-2 tree)
  (tree-map tree sq))
;(square-tree-2 '( 1 (2 (3 (4 (4 5) 5) (5 6))))) => '(1 (4 (9 (16 (16 25) 25) (25 36))))
;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (subsetstest s)
  (display "SS= ")
  (display s)
  (newline)
  (if (null? s)
      (list null)
      (let ((restlst (subsetstest (cdr s))))
        (display "restlst = ")
        (display restlst)
        (newline)
        (append restlst (map (lambda (elem)
                               (display "s= ")
                               (display s)
                               (display " ___(car s) = ")
                               (display (car s))
                               (display "+")
                               (display elem)
                               (display "--->")
                               (display (cons (car s) elem))
                               (newline)
                               (cons (car s) elem))
                             ;(list (car s) elem))
                             restlst)))))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((restlst (subsets (cdr s))))
        (append restlst (map (lambda (x)
                                (cons (car s) x))
                             restlst)))))
                        


;;2.2 sequences as conventional interfaces
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (sq tree) 0))
        (else
         (+ (sum-odd-squares (car tree))
            (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;;define a signal process
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))
;(filter odd? '(1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(accumulate + 0 '(1 2 3 4 5))
;(accumulate * 1 '( 1 2 3 4 5))


(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ 1 low) high))))

;;wrong - since this just creates a tree!
(define (enumerate-tree-wrong tree)
  (cond ((null? tree) null)
        ((not (pair? tree))
         tree)
        (else
         (cons (enumerate-tree-wrong (car tree)) ;this way we go through list of lists OR equivalent tree within a tree
               (enumerate-tree-wrong (cdr tree))))))
;;explore differnece between append and cons - subtle but one preserves the structure and append doesn't just gets all the elements -> append '(() (a b)) '((1 2 3) (2 3))
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree))
         (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

;;swee now we can start stacking these functions
(define (sum-odd-sq tree)
  (accumulate +
              0
              (map sq
                   (filter odd?
                           (enumerate-tree tree)))))
               
;(sum-odd-sq '(1 (2 (3 4) 5)))
(define (even-fibs2 n)
  (accumulate cons
              null
              (filter even?
                      (map fib (enumerate-interval 0 n)))))

;(even-fibs2 5)

(define (lst-fib-squares n)
  (accumulate cons
              null
              (map sq
                   (map fib
                        (enumerate-interval 0 n)))))


(define (product-of-squares-of-odd-elems seq)
  (accumulate *
              1
              (map sq
                   (filter odd? seq))))

;;map wtih accumulate!
;;2.33
;;think about how map works (if (predicate (car lst)) (cons (car lst) (map predicate (cdr lst))) (map predicate (cdr lst)))
(define (mymap2 p seq)
  (accumulate (lambda (x y)
                (if (p x)
                    (cons (p x) y)
                    y))
              null
              seq))

(define (myappend seq1 seq2)
  (accumulate cons
              seq2
              seq1))

;;what it's doing and why it works, car all the things in the main list and then just cons the entire second list
;(cons (car '(1 2 3))
;        (cons (car '(2 3))
;              (cons (car '(3))
;                    '(4 5 6))))

(define (mylength sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))

;;2.34
;;wow look how simple it is with honeer
(define (honer-eval x coefficient-sequence)
  (accumulate (lambda (this-coefficient higher-terms)
                (+ this-coefficient (* x higher-terms)))
              0
              coefficient-sequence))
;(= (+ 1 (* 2 3) (* 5 (expt 2 3)) (* 1 (expt 2 5)))
;   (honer-eval 2 '(1 3 0 5 0 1)))
;'(-6 3 -6 12 -4 7 -7 1 0 5 -2 -4 -12 2 7
;  12 -7 -10 -4 3 9 -7 0 -8 14 -3 9 2 -3 -10 -2 -6 1
;  10 -3 1 7 -7 7 -12 -5 8 6 10 -8 -8 -7 -3 9 1 6 6 -2
;  -3 -10 -2 3 5 2 -1 -1 -1 -1 -1 1 2 2 -1 -2 -1 0 1)
       
;;2.35
(define (count-leaves2 t)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              (enumerate-tree t)))

(define (count-leaves3-wrong t)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              (map (lambda (x)
                     (enumerate-tree x))
                   t)))

(define (count-leaves3 t)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

;but now if we're getting a a list like '(1 1 1 1 1 1 1 1) for each leaf we just have to add it up instead of like adding up the number of elements since it's the same thing!
(define (count-leaves4 t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

;(count-leaves4 '((1 (2)) 3 4))

;;ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (x) (car x)) seqs )) ;(we need to make a list with the first elemnets (map (lambda (x) (car x)) '((1 2 3) (4 5 6) (7 8 9))) => '( 1 4 7) -> goes into accumulate just a list 1 4 7 
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ;=> (22 26 30) ;just sums them all up

;;2.37
;'((1 2 3 4)
;  (4 5 6 7)
;  (6 7 8 9))

;;sum vi wi -> takes to vectors and returns a scalar quantity
(define (dot-product v w)
  (accumulate + 0 (map * v w)));
;(dot-product '(1 1) '(100 20)) => 120 


;;return a vector ti = sumj( mij vi)
(define (matrix-*-vector m v)
  (map (lambda (vec-m)
         (dot-product vec-m v))
   m))
;(matrix-*-vector '((100 200 300)
;                    (400 500 600))
;                  '(1 0 1 ))
;'(400 1000) 

;;return a matrix nik = mki (columns going down -> the thing we can access become rows going sideways using cons and accumulate)
(define (transpose m)
  (accumulate-n cons
                null
                m))
;(transpose '((1 2 3 4) (5 6 7 8) (9 8 7 6)))

;return a matrix p where pij = sumk(mik nki)
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v))
         m)))

;;correct = accumulate
(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (fold-right op initial (cdr seq)))))
;;combines the last element with all the elements on the left
;correct
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))


;;wrong
(define (left-fold op initial seq)
  (if (null? seq)
      initial
      (op (left-fold op initial (cdr seq))
          (car seq))))

;combines the first element with the result of combining all the elements to the right
;wrong
(define (right-fold op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial seq))
;(fold-left list null '(1 2 3)) ;=>  '(((() 1) 2) 3)
;(fold-right list null '(1 2 3)) ;=> '(1 (2 (3 ())))

;;2.39
(define (reverse2 seq)
  (fold-right (lambda (x y) (append y (list x))) null seq))

(define (reverse3 seq)
  (fold-left (lambda (x y) (cons y x)) null seq))


;;nested mapping - say we want a list of integer pair up to some number n that add up together to be a prime. (1 + 2) = 3 could be a pair

;;lets get a list of all the pairs first
;;and find the nested loops

;(right-fold append
;            null
;            (map (lambda (i)
;                   (map (lambda (j)
;                          (list i j)) ;;the result the pair of numbers we need accumulated
;                        (enumerate-interval 1 (- i 1)))); inner loop
;                (enumerate-interval 1 6))); in this scenario N = 6; our outer loop wiht

;;now we just need to filter the primes

;;combo of mapping and accumulating (fold-right) is so common that we can make it a procedure
(define (flatmap proc seq)
  (fold-right append null (map proc seq))) ;(map list '(1 2 3)) => '((1) (2) (3))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
              (flatmap (lambda (i)
                         (map (lambda (j)
                                (list i j))
                              (enumerate-interval 1 (- i 1))))
                       (enumerate-interval 1 n)))))



;;key is we reduce by computing S - x
(define (remove-2 x lst)
  (cond ((null? lst) null)
        ((= x (car x))
         (remove-2 x (cdr lst)))
        (else
         (cons (car lst) (remove x (cdr lst))))))
;;filter everythiing that's not x - (same as removing x - smart)
(define (remove1 x lst)
  (filter (lambda (elem)
            (not (= elem x)))
          lst))

;;wrong right now see what's going on
(define (perms1 s)
  (display "s=")
  (display s)
  (display " ----- ")
  (if (null? s)
      (list null) ;(map (lambda (x) (display "in lambda function")) null); => '() as in it doesn't go into the lambda function so it's just blank - this is why we need (list null) instad of null -- try with 'a '(a) and '((a)) to see differnce
      (fold-right append
                  null
                  (map (lambda (x)
                         (map (lambda (p)
                                (display " x= ")
                                (display x)
                                (display "+")
                                (display p)
                                (display "=")
                                (display (cons x p))
                                (newline)
                                (cons x p))
                              (perms1 (remove1 x s))))
                       s))))

;21290005252814

;;the wrong way i was thinknig about
(define (perms2 s)
  (display "s=")
  (display s)
  (display " ----- ")
  (if (null? s)
      (list null)
      (map (lambda (x)
             (map (lambda (p)
                    (display " x= ")
                    (display x)
                    (display "+")
                    (display p)
                    (display "=")
                    (display (cons x p))
                    (newline)
                    (cons x p))
                  (perms2 (remove x s))))
             s)))

;(accumulate append null '(((1))))

;diff ends up being (accumulate append null '((2))); => '(2) instead of '((2)) so the next time around it's (cons 1 '(2)) instead of (cons (

(define (permutations s)
  (if (null? s)
      (list null);'(())
      (flatmap (lambda (x)
                 (map (lambda (perm-ret)
                        (cons x perm-ret))
                      (permutations (remove x s))))
               s)))

;;2.40 unique pairs where (1 <= i <= j <= n)
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;ex 2.41 - ordered tripels - distinct pos integers i,j,k < n and = to some sum s
(define (triples-sum n some-sum)
  (permutations (filter (lambda (trip)
                          (and (= (length trip) 3)
                               (= (apply + trip) some-sum)))
                        (subsets (enumerate-interval 1 n)))))


;2.42 - 8 queens -put queen in each column, once we've put k-1 queens. For each of these ways,
;generate an extended set of positions by placing a queen in each row of the kth column.
;Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens.
;This produces the sequence of all ways to place k queens in the first k columns.
(define (queens board-size)
  (define (queens-col k) ;seq of all ways to place queens in the first k columns of the board
    (if (= k 0)
        (list empty-board)
        (let ((ret (filter (lambda (positions)
                             ;(display-stuff k positions)
                             (safe? k positions)); a postion is just a (row column) position of a single queen, positions (plural) means all the queens on a single board
                           (let ((all-positions (flatmap (lambda (rest-of-queens) ;way to place k-1 queens           
                                                           (map (lambda (new-row) ;queen for the kth column, the one we're testing out
                                                                  (adjoin-position new-row  k rest-of-queens)) ;
                                                                (enumerate-interval 1 board-size)));since we want to put it in each row
                                                         (queens-col (- k 1)))));;imagine only a list of queens-in columns where they aren't intersected, now just get all possible combinations 
                             (display-stuff "all-positions: " all-positions)
                             all-positions)))); like the kth column,like from the 8th columsn
          (display-stuff "ret=" ret)
          ret)))
  (queens-col board-size))

;;positions are just row column pairs - easy to represent
(define (make-position row col)
  (cons row col))
(define (position-row pos)
  (car pos))
(define (position-col pos)
  (cdr pos))

(define (list-ref lst n)
  (cond ((null? lst)
         null)
        ((= n 0) (car lst))
        (else
         (list-ref (cdr lst) (- n 1)))))
          
(define empty-board null)

(define (adjoin-position row column positions)
  (append positions (list (make-position row column))))

(define (display-stuff x y)
      (newline)
      (display x)
      (display " || ")
      (display y)
      (newline))

;give a column and a list of postions of queens - show me a queen in the col column is safe relative to the placement of the other queens
(define (safe? col positions)
  (let ((kth-queen (list-ref positions (- col 1)))
        (other-queens (filter (lambda (q) ;don't look at anything in the same colum
                                (not (= col (position-col q))))
                              positions)))
    (define (attacks? q1 q2)
      (or (= (position-row q1) (position-row q2)) ;nothing in the same row
          (= (abs (- (position-row q1) (position-row q2))) ;nothing in the same diagonal
             (abs (- (position-col q1) (position-col q2))))))

    (define (iter q board)
      (or (null? board)
          (and (not (attacks? q (car board)))
               (iter q (cdr board)))))
    (iter kth-queen other-queens)))


;;sicp-deriv.rkt - that file ahs our algebraic expressions
;;to define a set! do a method of data abstraction -> define operations to be used on a "set"
;;union-set, intersection-set, element-of-set?
;;adjoin-set
;;;start with just a list and lets see some cost benefits



    