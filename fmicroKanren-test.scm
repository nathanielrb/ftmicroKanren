;; Tests for Chicken Scheme

(use srfi-1)

(define (assp pred alist)
  (find (lambda (pair) (pred (car pair))) alist))

(include "fmicroKanren.scm")
(include "miniKanren-wrappers.scm")
(include "microKanren-test-programs.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (format #t
              "Failed: ~a~%Expected: ~a~%Computed: ~a~%~%"
              'tested-expression expected produced)))))))


(define (==later a b)
  (lambda (s/c)
    (delay
      ((== a b) s/c))))

(define empty-state '(() . 0))

(test-check "disj with one later, current"
  (take-now ((call/fresh (lambda (q) (disj (== q 4) (==later q 5)))) empty-state))
  '((((#(0) . 4)) . 1)))

(test-check "disj with one later, promised"
  (force (cdr ((call/fresh (lambda (q) (disj (== q 4) (==later q 5)))) empty-state)))
  '((((#(0) . 5)) . 1)))

(test-check "conj with one later"
  (promise? ((call/fresh (lambda (q) (conj (== q 4) (==later q 5)))) empty-state))
  #t)

(test-check "unsuccessful conj with one later, promised"
  (force ((call/fresh (lambda (q) (conj (== q 4) (==later q 5)))) empty-state))
  '())

(define $0 (run* (q) (disj (== q 4) (next (== q 5)))))

(test-check "miniKanren disj with one later, current"
  (take-now $0)
  '(4))

(test-check "miniKanren disj with one later, promised"
   (take-next $0)
  '(5))

(test-check "basic functioning of disj"
 (take-now ((disj (next (== #(0) 6)) (disj (== #(0) 5) (next (== #(0) 7)))) empty-state))
 (take-now ((disj (== #(0) 5) (next (disj (== #(0) 6) (== #(0) 7)))) empty-state)))

(define $1
  (run* (q)
    (next (== q 4))
    (disj (next (next (== q 5)))
          (next (== q 4)))))

(test-check "nested promises, current"
  (promise? $1) #t)

(test-check "nested promises, promised"
  (take-now (take-next $1))
  '(4))

(test-check "nested promises, twice promised"
  (take-next (take-next $1))
  '())

(define (inco x)
  (let r ((n 0))
    (disj (== x n) (next (r (+ n 1))))))

(define $2
  (run* (q)
    (fresh (a b)
      (== (list a b) q)
      (conj (inco a) (inco b)))))

(test-check "recursively nested promises, current"
  (take-now $2)
  '((0 0)))

(test-check "recursively nested promises, first promise"
  (take-now (take-next $2))
  '((0 1) (1 0) (1 1)))

(test-check "recursively nested promises, second promise"
  (take-now (take-next (take-next $2)))
  '((1 2) (2 0) (2 1) (2 2) (0 2)))

(test-check "recursively nested promises, third promise"
  (take-now (take-next (take-next (take-next $2))))
  '((0 3) (2 3) (3 0) (3 1) (3 2) (3 3) (1 3)))


;;;; Original microKanren tests

(newline)(print "Original microKanren tests")
(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (car $))
  '(((#(0) . 5)) . 1))

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (car $))
  '(((#(1) . 5) (#(0) . 7)) . 2))

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (take 1 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (car (cdr $)))
  '(((#(1) . 6) (#(0) . 7)) . 2))

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take 2 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take-all $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "ground appendo"
  (car ((ground-appendo empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "ground appendo2"
  (car ((ground-appendo2 empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "appendo"
  (take 2 (call-appendo empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
    (((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5))) . 7)))

(test-check "appendo2"
  (take 2 (call-appendo2 empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4) (((#(0) #(1) #(2) #(3)) (#(3) #(4) . #(6)) (#(2) . #(6)) (#(5)) (#(1) #(4) . #(5))) . 7)))

(test-check "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "many non-ans"
  (take 1 (many-non-ans empty-state))
'((((#(0) . 3)) . 1)))

;; of course db *should* be a real mk generator, but for simplicity's sake..
(define *db* 1)
(define (db x) (Zzz (== x *db*)))
(define r1 (run* (q) (== q 'done) (until (db 1) (db 3))))
(define r2 (run* (q) (== q 'done) (precedes (db 3) (db 4))))
(set! *db* 3)

(test-check "Simple 'until' that succeeds"
  (take-next r1) '(done))

(test-check "Simple 'precedes' that succeeds"
  (take-next r2) '(done))



