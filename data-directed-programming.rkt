#lang racket

;;data directed programming
;;if deal with set of generic operations(mag ang imag-part real-part) common to a set of differnt types (polar rect), we can think of it as a 2-d table

;;operations vs type
;           | polar            | rect           | type 3 | type 4| type 5
;real-part  | real-part-polar  |real-part-rect  |        |       |     
;imag-part  | imag-part-polar  |imag-part-rect  |        |       |
;mag        | mag-polar        |mag-rect        |        |       |
;angle      | ang-polar        |ang-rect        |        |       |
;operation5 |                  |                |        |       |
;operatoin6 |                  |                |        |       |
;;so the function we need is to find the operaton name + the type and return the procedure!

;;(put -> puts item in operation-type tbl)
;;(get -> gets a line from the operation->type tbl)

(define (sq x) (* x x))
(define (put operation tag procedure) null)
(define (get operation tag) null)
;;recall
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;;package 1
(define (install-rect-package)
  ;;internal procedure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z) (sqrt (+ (sq (imag-part z))
                           (sq (real-part z)))))
  (define (ang z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;;interface to rest of the system
  (define (tag x) (attach-tag 'rect x))
  (put 'real-part '(rect) real-part); ;operation '(type) proecdure
  (put 'imag-part '(rect) imag-part)
  (put 'magnitude '(rect) mag)
  (put 'angle '(rect) angle)
  (put 'make-from-real-imag '(rect)
       (lambda (x y)
         (make-from-real-imag x y)))
  (put 'make-from-mag-ang '(rect)
       (lambda (r a)
         (make-from-mag-ang r a)))
  'done)

;package 2
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
(define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (sq x) (sq y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
`done)

;;remember type-tag

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;we want '(rect) instead of 'rect to work with get function?
    (let ((proc (get op type-tags))); since it looks like get needs a list as the second arg? 
      (if proc
          (apply proc (map contents args)) ;apply needs a list
          (error "no method for these types -- apply-generic" (list op type-tags))))))

;(type-tag '(rect (cons 3 4))) ;-> rect
(map type-tag (list (attach-tag 'rect (cons 3 4)))); -> '(rect)

;;use apply-generic to get our generic selectors
(define (real-part z) (apply-generic 'real-part z)) ;;now what does z look like? depends on it's type '(rect (cons 3 4)) or it could be '(polar (cons 5 (atan 4 3)))
(define (imag-part z) (apply-generic 'imag-part z))
(define (mag z) (apply-generic 'mag z))
(define (ang z) (apply-generic 'ang z))

;;constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rect); how come a symbol in the second arg doesn't mess it up - assume get takes care of it? 
   x
   y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;;2.73 - remember derivatives from sicp-deriv.rkt

;;make a variable a symbol
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2) (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (operands x) (cdr x))
(define (operator x) (car x))
(define (sum? e) (and (pair? e) (eq? (car e) '+))) 
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (addend exp) (second exp))
  (define (augend s)
    (if (<= (length s) 3);has 2 things then do what we always do show the second arg, like make it look like a pair
        (third s)
        (list '+ (third s) (augend (cdr s)))))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
(define (multiplier e)
  (second e))
(define (multiplicand e)
  (if (<= (length e) 3)
      (third e)
      (list '* (third e) (multiplicand (cdr e))))); get a say it's (* a b (+ 1 2) 4) => (* b (* (+ 1 2) 4)); which is the augend in the format that woked before
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) ;some simplifications
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list '* m1 m2)))) ;what we actually want



(define (deriv-dispatch exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-dispatch (addend exp) var)
                   (deriv-dispatch (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-dispatch (multiplicand exp) var))
           (make-product (deriv-dispatch (multiplier exp) var)
                         (multiplicand exp))))
        ;<more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

;;see how it gets simplified to this
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)); z (dy/dx) = 0
        (else
         ((get 'deriv (operator exp)) ;looks for a (+ expression) or a (* expression) or some other prefix notation to call a function with 
          (operands exp)
          var))))

;;look at variable? it's just a functon that takes something to see, it doesn't have a car and a cdr to help us determie which function from the table to choose

;;get takes two keys could we do someting like (put 'var 'null (lambda (x) (symbol? x))

;;so the sum? and product? they rely on using prefix notation '(+ 3 x) or '(* 3 x) --> car is the symbol that tells us which one to dispatch, but number? and variable? can
;not rely on the car and the cdr for exp, since in these scenarios there is no car or cdr

;;we want to do something like
;(get 'deriv (operator exp)); and have this map to some function (lambda (exp var) 0) always returns 0 if it's a number -> (get 'deriv 'number) need to do a (put 'deriv 'number (lambda (exp var) 0))

;;takes 2 things and returns 0
;;instead we wolud be forced to do somethng static like this
;(define (operand exp)
;  (cond ((number? exp)
;         (list 'number exp)); we want the first thing to be similar to prefix notation like a (+ 3 x) or a (** 3 y)
;        ((variable? exp)
;         (list 'variable exp))
;        (else
;         (second exp))))
;(define (operator exp) (cdr exp)) ;;if it's a number we want it to be a list

;;see how with this we get it into a format like '(number 3)
;(get 'deriv (operator exp)); -> (get 'deriv 'number) and have that return number? no makes more sense to (lambda (x y) (number? x) 0) makes the most sense

;;works if we do a put now we just need it to refer to a variable that takes 2 things
;(define (number?-dispatch exp var)
;  0) ;since we 
;(define (variable?-dispatch exp var) ;(takes a '(variable x) expression like that and so operands would result in '(x) we need the car of this expression
;  (if (same-variable? (car exp) var) 0 1))

;;2.73b need to create a package

(define (install-sum-package)
  (define (addend exp) (second exp))
  (define (augend s)
    (if (<= (length s) 3);has 2 things then do what we always do show the second arg, like make it look like a pair
        (third s)
        (list '+ (third s) (augend (cdr s)))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else
           (list '+ a1 a2))))
   (define (deravitive exp var) ;name it something other than deriv (that's the only thing that can confuse it, call it deriv-sum to make this super obvious what's going on in the put later
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '+ deravitive))


;put all the mulitplicaiton stuff in there, like we did above (just assume it's in there) - since it's in there
(define (install-multiplication-package)
  (define (deravitve exp var)
    (make-sum
     (make-product (multiplicand exp) (deriv (multiplier exp) var))
     (make-product (deriv (multiplicand exp) var) (multiplier exp))))
  (put 'deriv '* deravitve))

;;2.73c -> puts this
(define (install-exponent-package)
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
           (list '** b n))))
  (define (deravitive exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                                (deriv (base exp) var))))
  (put 'deriv '** deravitive))

;;if we wanted to do
;((get (operator exp) 'deriv)
; (operands exp) ;same order
; var) ;same order

;;all we have to do is change the put
;(put '+ 'deriv function);; just changing one thing around in the put will fix everything


;2.74
;;with this company, it'll be the same idea, divs have a file (differnt format), sorted by employee name (or empID) but can be in any format.
;;each file has records keyed on employee names (seems silly would probably be an employee id incase 2 of them have hte same name, then you need another way to find them).

(get 'deriv '+);;-> function name and a type

(get 'sometthing 'div-name) ;since each division name (or division-id) is unique



;;does a (get 'emp-id 'div-name) should that return a function? that meakes no sense, probably need a functon like in the above example
;;get-record that retrieves employee record from a specified file
;;so first how to get a list of employee records
(get 'get-record 'division-name); -> that should get us the right list of records specified by employee name ((emp1 ....) (emp2 ....)) and it would be different if it's a list or of they stored it as
;a hash, above i'm just assuming an un-ordered list is how the file is arranged. So the division's files should have some sort of thing letting us know it's a hash or whatever?
;;we'd need to tag each data strcutre for the file with the right type-tag 'hash 'ol 'ul 'binary-tree 'red-black-tree

(define (get-record employee-name personnel-file)
  (let ((record ((get 'get-record (type-tag personnel-file)) ;;the get-r'get ecord function for the particular division (from the type tag) will return the right get-record function to help 
                 employee-name ;use the employee name and the personell file to get record, each get-record will know how to pick this based on the division
                 personnel-file)))
    (if record
        (attatch-tag (type-tag peronnel-file) record) ;;ie we want the division name returned for the record with the employee id
         #f)))


;;now get salary just get a record and now do the same thing
(define (get-salary record)
  ((get 'get-salary (type-tag record)) ;assume the record has the division else how can get-salary work since it won't know the structure this division uses
   (contents record)));;ie get everyhing but 

;;see we couldn't do part b like this
;(define (get-salary employee-name)
;  (let ((record (get-record employee-name 'some-division))) ;;see how we still need the division here to break this down, since each record can have a different retrieval method
;    ((get 'get-salary (type-tag record))
;     (contents record))))

;2.74 c -- just like a regular iterate
(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      null
      (let ((record (get-record employee-name (car division-files)))) ;will return with an employee id (since it has the division tag already on it) or false if it doesn't find it
        (if record
            record
            (find-employee-record employee-name (cdr division-files))))))
;;so now if they get a new file
;(put 'get-salary 'new-division (lambda ...))
;(put 'get-record 'new-division (lambda (employee-name division) (+ 1 1)));







  
      




