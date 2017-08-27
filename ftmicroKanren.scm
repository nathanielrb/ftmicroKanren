;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

;; extended for first-order temporal logic using promises
;; by Nathaniel Rudavsky-Brody

(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    ((and (promise? $1) (promise? $2)) 
     (delay (mplus (force $1) (force $2))))
    ((promise? $1) (mplus $2 $1))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (advance g)
  (lambda (s/c)
    (let rec (($ (g s/c)))
      (cond ((null? $) '())
	    ((promise? $) (force $))
            ((procedure? $) (lambda () (rec ($))))
	    (else (cons (car $) (rec (cdr $))))))))

(define (bind $ g)
   (cond
     ((null? $) mzero)
     ((procedure? $) (lambda () (bind ($) g)))
     ((promise? $) (delay (bind (force $) (advance g))))
     (else (mplus (g (car $)) (bind (cdr $) g)))))

(define-syntax next
  (syntax-rules ()
    ((_ g) (lambda (s/c) (delay (g s/c)))))) 


