#lang racket
(require graphics/graphics)
(open-graphics)

(define y-max 500); we need to do someting like (- y-max ycoord) to get
(define x-max 500)
(define vp (open-viewport "A Picture Language" x-max y-max))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
;(define (vector-to-posn v)
;  (make-posn (car v) (- y-max (car(cdr v))))); (0,0) starts in the top left cornder so lets fix that
(define (vector-to-posn v)
  (make-posn (car v) (- y-max (cadr v))))


(define (make-frame origin e1 e2)
  (list origin e1 e2))

;;frame coordinate map
;;takes a frame - frame is something with 3 vectors (origin (vector from (0 0)), edge1 (horizontal edge), edge2 (vertical edge))
;;gives a vector that will be scaled and transformed to fit the frame
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (segments->painter segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)
       (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

;;segment can be just 2 vectors
(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cadr s))

(define (enumerate-interval a b)
  (if (> a b)
      null
      (cons a (enumerate-interval (+ a 1) b))))

(define (for-each proc lst)
  (cond ((null? lst)
         null)
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
      
       
;;Picture language - Focus on the langauge's primitives, it's means of combination, and it's means of abstraction
;;element called a painter - a function that puts a DRAWING (line) on a FRAME (parallelogram scales and shifts the image)

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;(clear)
;((flip-horiz wave-painter) frame2)

(define (below painter1 painter2)
  (flip-horiz (rotate90 (beside (flip-horiz (rotate90 painter1))
                                (flip-horiz (rotate90 painter2))))))

(define (rotate270 painter)
  ((repeat rotate90 3) painter))
   
;;f(g(x))
(define (compose f g)
  (lambda (x)
    (f (g x))))
;;;repeat at least one time
(define (repeat f num-times)
  (define (rec num acc)
    (if (<= num 1)
        acc
        (rec (- num 1) (compose f acc))))
  (rec num-times f))

(define wave null)


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin ;new origin (as the name suggests)
                             (sub-vect (m corner1) new-origin); new edge1
                             (sub-vect (m corner2) new-origin))))))); new edge2
;;the reason this still works is because 
(define (flip-vert painter)
  (transform-painter painter
                    (make-vect 0 1.0) ;new origin is the top left of the screen
                    (make-vect 1.0 1.0)  ;new end of edge1 - just the same as before, points the same way, points right
                    (make-vect 0.0 0.0))); new end of edge2 - see how it's the same angle just pointing down




;takes 2 painters and produces a new compound painter that is painter one on the left and painter 2 on the right half of the frame
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
;;abstract the above pattern
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
;simple now to define wave4
(define (wave4-alternate painter)
  (flipped-pairs painter))
;;do a recursive pattern like so
(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (- n 1)))) ;way it gets smaller is by what's next to it on the line below, seems like it implicitly has a frame
        (beside painter (below smaller smaller)))))
;besides takes 2 things, which are the results of right-split which could just be the painter itself!!! got it!!!!!! ah ha!!!

;;ex 2.44
(define (up-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller))))) 


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (up    (up-split painter (- n 1))))
        (let ((bottom-right (below right right));notice two right-splits below each other actually does create the pattern we want creates a pattern that we want, like it looks like half each time
              (top-left (beside up up))
              (top-right (corner-split painter (- n 1))))
          (below (beside painter bottom-right)
                 (beside top-left top-right))))))

;;see how easy it is to build up
;((corner-split wave-painter 1) frame2); use that to confirm what you're thinking, using it with 2 works


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;((square-limit wave-painter 2) frame2)

;;higher order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;(((square-of-four identity flip-vert identity flip-vert) wave-painter) frame2)

;;so make flipped pair using the above
(define (flipped-pairs2 painter)
  ((square-of-four identity flip-vert
                  identity flip-vert)
   painter))
  

(define (square-limit2 painter n)
  ((square-of-four flip-horiz identity
                  rotate-180 flip-vert); this returns a function that takes a painter
   (corner-split painter n))); corner-split returns a painter

(define (rotate-180 p)
  (flip-vert (flip-horiz p)))

;;so we need split to be a higher order operator
;;so right away we need it to return a funciton!, it'll be a function used to produce up-split and right split meaning
;;we need to return a function that has a signature like right-split and up-slit
(define (split outer inner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split outer inner) painter (- n 1)))); (split outer inner) rets a function that takes 2 variables that we also pass it, to make it smaller
          (outer painter (inner smaller smaller))))))
;;now to use split
(define right-split2 (split beside below))

(define up-split2 (split below beside))

;wrong because i was just trying to return a painter here, not a way to transform paintner           
;;(define (split painter n outer-overall inner-small)
;;  (if (= n 0)
;;      paitner
;;      (let ((smaller (split painter (- n 1) left right)))
;;        (outer-overall painter (inner-small smaller smaller)))))

;;a frame is 3 vectors,
;origin vector (0,0), horiz vector (1 1), vert vector, (0 1) - this would give us something like a 


;(map (lambda (endpoint)
;         ((draw-line vp) (vector-to-posn (make-vect 0 (- 500 0)))
;                         (vector-to-posn (make-vect endpoint (- 500 500)))))
;       (enumerate-interval 200 500))

(define fr (make-frame '(0 0) (make-vect 1 0) (make-vect 0 1)))
((frame-coord-map fr) '(12 23)); '(12 23) since the above is a perfect frame rectanle

(define myframe (make-frame '(0 0) '(500 0) '(0 500)))
((frame-coord-map myframe) '(.1 .2))


;2.49 b; make an x
(define s1 (make-segment '(0 0) '(500 500)))
(define s2 (make-segment '(500 0) '(0 500)))
(define segs (list s1 s2))
;((segments->painter segs) fr)

;;make just the edges
;((segments->painter '( ((0 0) (0 500)) ((0 500) (500 500)) ((500 500)(500 0)) ((500 0)(0 0))))
; fr)
;;make a diamond
;((segments->painter '(((0 250) (250 500))
;                      ((250 500) (500 250))
;                      ((500 250)(250 0))
;                      ((250 0)(0 250))))
; fr)

(define (scale-segment k seg)
  (make-segment (scale-vect k (start-segment seg))
                (scale-vect k (end-segment seg))))
;;make a wave pattern
(define wave-segs
  (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))))

(define wave-segs2 (map (lambda (s) (scale-segment 500 s))
                        wave-segs))
;((segments->painter wave-segs2) fr)

;;can mess with each of the variables in the frame and see what happens
(define (my-draw testnum)
  (let ((myframe (make-frame '(0 0) (make-vect 1 0) (make-vect (/ testnum 10) 1))))
    ((segments->painter wave-segs2) myframe)))


;(for-each (lambda (num) (my-draw num))
;            (enumerate-interval 0 0))

;;this is a procedure that takes any frame and draws it :)
(define wave-painter-scaled (segments->painter wave-segs2));using the scaled segments - this is a painter
(define wave-painter (segments->painter wave-segs));see how this is a painter, it takes a frame and now projects stuff to it
;;both of the above are wrong because we're 


(define frame2 (make-frame (make-vect 0 0) ;origin
                            (make-vect x-max 0); edge1
                            (make-vect 0 y-max))); edge2

(wave-painter frame2); see how it prints something
;lets have fun and scale it and mess with it, this is a frame that's been tilted
((segments->painter (list '((.5 .3) (1.0 .3)))) (make-frame (make-vect 0 0)
                                                              (make-vect 400 300); 30 degree vector with size 5 right
                                                              (make-vect 0 500)))
;;more clear example
(clear)
(wave-painter (make-frame (make-vect 0 0)
                          (make-vect 400 300); 30 degree vector with size 5 right
                          (make-vect 0 500)))
;notice how it's a 30 degree angle like we expect
((segments->painter (list '((0 0) (1 0)))) (make-frame (make-vect 0 0)
                                                       (make-vect 200 150); 30 degree vector with size 5 right
                                                       (make-vect 0 250)))

;transform painter takes a painter and will return a painter (something that takes a frame)
(define (transform-painter-duplicate painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map  frame)))
      (let ((new-origin (m origin))); now the origin is transformed so that it shows up on the frame correctly
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin); edge1 (the horiz edge) relative to the new origin - that is scaled to fit the frame
                             (sub-vect (m corner2) new-origin))))))) ;edge2 (the vert edge) scaled to fit the frame relative to the new origin that is also going to fit the frame
(clear)
((flip-vert wave-painter) frame2)




(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect .5 .5)
                     (make-vect 1.0 .5)
                     (make-vect .5 1.0)))
(clear)
((shrink-to-upper-right wave-painter) frame2)

(clear)
((shrink-to-upper-right (rotate90 wave-painter)) frame2)

(define (squish-inswards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect .65 .35)
                     (make-vect .35 .65)))


          