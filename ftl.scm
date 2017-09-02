;; Example of first-order temporal logic programming

(use srfi-1)

(define (assp pred alist)
  (find (lambda (pair) (pred (car pair))) alist))

(load "ftmicroKanren.scm")
(load "miniKanren-wrappers.scm")
(load "ftmicroKanren-test.scm")

(define (fail g)
  (lambda (s/c)
    (let (($ (pull (g s/c))))
      (if (null? $) (unit s/c) mzero))))

(define (guard* g)
  (lambda (s/c)
    (let loop (($ (g s/c)))
      (cond ((null? $) (unit s/c))
	    ((procedure? $) (lambda () (loop ($))))
	    ((promise? $) (delay (loop (force $))))
	    ((equal? (caar $) (car s/c)) mzero)
	    (else (loop (cdr $)))))))

;; (define (guard g1 g2)
;;   (conj g2 (rguard g1)))

(define (guard g1 g2)
  (lambda (s/c)
    (let loop (($ (g1 s/c)))
      (cond ((null? $) (g2 s/c))
	    ((procedure? $) (lambda () (loop ($))))
	    ((promise? $) (delay (loop (force $))))
	    ((equal? (caar $) (car s/c)) mzero)
	    (else (loop (cdr $)))))))

(define (guard2 g1 g2)
  (lambda (s/c)
    (letrec ((check
	      (lambda (s/c)
		(let loop (($ (g1 s/c)))
		  (cond ((null? $) s/c)
			((procedure? $) (lambda () (loop ($))))
			((promise? $) (delay (loop (force $))))
			((equal? (caar $) (car s/c)) mzero)
			(else (loop (cdr $))))))))
      (if (null? (check s/c))
	  (bind (g2 s/c) check)
	  mzero))))

(define (guard3 g1 g2)
  (lambda (s/c)
    (letrec ((check
	      (lambda (s/c)
		(let loop (($ (g1 s/c)))
		  (cond ((null? $) s/c)
			((procedure? $) (lambda () (loop ($))))
			((promise? $) (delay (loop (force $))))
			((equal? (caar $) (car s/c)) (loop (cdr $)))
			(else (cons (car $) (loop (cdr $)))))))))
      (bind (g2 s/c) check))))

(define (guard4 g1 g2) (conj g2 (fail g1)))
    
(define-syntax always
  (syntax-rules ()
    ((_ g) (always* (lambda () g)))))

(define (always* g*)
  (conj (g*) (next (always* g*))))
(define-syntax until
  (syntax-rules ()
    ((_ g1 g2) (until* (lambda () g1) (lambda () g2)))))

(define (until* g1* g2*)
  (disj+ (g2*) 
	 (conj (g1*) (next (until* g1* g2*)))
	 (next (until* g1* g2*))))

(define (until* g1* g2*)
  (letrec ((U (lambda (g1) (disj (g2*) (conj g1 (next (U g1)))))))
    (disj (let ((g1 (g1*)))
	    (conj g1 (next (U g1))))
		  
	  (next (until* g1* g2*)))))

(define-syntax precedes
  (syntax-rules ()
    ((_ g1 g2) (precedes* (lambda () g1) (lambda () g2)))))

;; ifte
;; (define (precedes* g1* g2*)
;;   (disj (g1*)
;; 	(guard (g2*)
;; 	  (next (precedes* g1* g2*)))))


(define-syntax eventually
  (syntax-rules ()
    ((_ g) (eventually* (lambda () g)))))

(define (eventually* g*)
  (let ((g (g*)))
    (disj g
	  (guard g
	    (next (eventually* g*))))))

;; (define (precedes* g1* g2*)
;;   (let ((g1 (g1*))
;; 	(g2 (g2*)))
;;     (disj+
;;      (conj g1 	   
;; 	   (guard g2
;; 	     (eventually (g2*))))
;;      (guard (disj g1 g2) (next
     ;; (next (guard g2
     ;; 	     (precedes* (lambda () g1) g2*)))
     ;; (next (guard g2
     ;; 	     (precedes* g1* g2*))))))

(define (precedes* g1* g2*)
  (let ((g1 (g1*))
	(g2 (g2*)))
    (disj (conj g1 (eventually* g2*))
	  (next (precedes* g1* g2*)))))




(define *db* (make-parameter 1))

(define (db x) 
  (begin (print "checking " x " against " (*db*))
	 (== x (*db*))) )

(define r0 (run* (q) 
	     (fresh (a b)
               (== q `(,a ,b)) 
	       (precedes (db a) (db b)))))

(define r1 (run* (q)
	     (fresh (a b)
	       (eventually (db b))
	       (precedes (db a) (db b))
               (== q `(,a ,b)) 
	       )))

(define r2 (run* (q)
	     (fresh (a b)
	       (precedes (db a) (db b))
	       (eventually (db b))
	       (== q `(,a ,b))
	       )))

(define r3 (run* (q) (fresh (a b) (== q `(,a ,b)) (until (db a) (db b)))))

(newline)
(print "r0: " r0)
(print "r1: " r1)
(print "r2: " r2)
(print "r3: " r3)

(*db* 2) 

(newline)
(define r0* (advance r0)) (print "r0: " r0*)
(define r1* (advance r1)) (print "r1: " r1*)
(define r2* (advance r2)) (print "r2: " r2*)
(define r3* (advance r3)) (print "r3: " r3*)

(*db* 3)

(newline)
(define r0** (advance r0*)) (print "r0: " r0**)
(define r1** (advance r1*)) (print "r1: " r1**)
(define r2** (advance r2*)) (print "r2: " r2**)
(define r3** (advance r3*)) (print "r3: " r3**)

;;; examples 

;; (define r0 (run* (q) (== q 'success) (until (db 1) (db 3))))
;; ;; r0 => #<promise>

;; (define r1 (run* (q) (== q 'success) (until (db 1) (db 4))))
;; ;; r1 => #<promise>

;; (define r2 (run* (q) (== q 'success) (precedes (db 3) (db 4))))
;; ;; r2 => #<promise>

;; (define r3 (run* (q) (== q 'success) (precedes (db 1) (db 3))))
;; ;; r3 => (success . #<promise>)

;; (define r4 (run* (q) (== q 'success) (precedes (db 2) (db 3))))
;; ;; r4 => #<promise>

;; (define r5 (run* (q) (== q 'success) (conj 
;; 					(precedes (db 1) (db 3))
;; 					(eventually (db 3))
;; 				      )))

(print "\nFTL Tests")

;; (test-check "successful until"
;;   (force r0) '(success))

;; (test-check "unsuccessful until"
;;   (force r1) '())

;; (test-check "immediately successful precedes"
;;   (car (force r2)) 'success)

;; (test-check "successful precedes"
;;  (car (force r3)) 'success)

;; (test-check "unsucessful precedes"
;;   (force r4) '())
