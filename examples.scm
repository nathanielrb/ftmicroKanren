;; Example of first-order temporal logic programming
;; using fail as negation

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

(define (maybe g)
  (lambda (s/c)
    (cons s/c (g s/c))))

;; (define (forward g1 g2)
;;   (disj g1 (conj (fail g1) g2)))

(define (forward g1 g2)
  (lambda (s/c)
    (let (($ (pull (g1 s/c))))
      (if (null? $) (g2 s/c) $))))

(define (always g)
  (conj g (next (always g))))

(define (until g1 g2)
  (forward g2 (conj g1 (next (until g1 g2)))))

(define-syntax precedes
  (syntax-rules ()
    ((_ g1 g2) (precedes* (lambda () g1) (lambda () g2)))))

(define (precedes* g1* g2*)
  (disj (g1*)
	(conj (fail (g2*))
	      (next (precedes* g1* g2*)))))

(define-syntax eventually
  (syntax-rules ()
    ((_ g) (eventually* (lambda () g)))))

(define (eventually* g*)
  (disj (g*) 
	(conj (fail (g*))
	      (next (eventually* g*)))))

(define (fp $) (force (promised $)))

(define *db* (make-parameter 1))

;;(define (db x) (Zzz (begin (print "checking " x) (== x *db*))))
;; (define (db x)
;;   (let ((d (*db*)))
;;     (lambda (s/c)
;;       (lambda ()
;; 	(print "checking " x)
;; 	((== x d) s/c)))))

(define (db x) (begin (print "checking " x " against " (*db*)) (== x (*db*))))

(define r5 (run* (q) (== q 'success) (conj (precedes (db 1) (db 3)) (eventually (db 3))  )))
(define r6 (run* (q) (== q 'success) (conj (eventually (db 3)) (precedes (db 1) (db 3)) )))

(*db* 3)

;; (define (precedes2 g1 g2)
;;   (forward (conj g1 (maybe (next (eventually g2))))
;; 	   (conj (fail g2)
;; 		 (next (precedes g1 g2)))))

(define (fp $) (force (promised $)))
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

(newline)
(print "FTL Tests")

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
