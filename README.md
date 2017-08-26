# ftKanren

A first experiment in implementing first-order logic programming in miniKanren, written as an extension of Jason Hemann and Daniel P. Friedman's microKanren.

The basic temporal primitive `later` is implemented using delayed streams (Scheme promises), alongside immature and mature ones.

## Primitives

`next` `always` `eventually` `until` `precedes`


## Implementation Details

Here is a low-level example showing the interaction between promises, `conj` and `disj`.

```
(define (==next a b)
  (lambda (s/c)
    (delay
      ((== a b) s/c))))

(define empty-state '(() . 0))

((call/fresh (lambda (q) (disj (== q 4) (==next q 5)))) empty-state)
;; => ((((#(0) . 4)) . 1) . #<promise>)

(force (cdr ((call/fresh (lambda (q) (disj (== q 4) (==next q 5)))) empty-state)))
;; => ((((#(0) . 5)) . 1))      	

((call/fresh (lambda (q) (conj (== q 4) (==next q 5)))) empty-state)
;; => #<promise>

(force ((call/fresh (lambda (q) (conj (== q 4) (==next q 5)))) empty-state))
;; => ()
```

The macro `next` is defined to facilitate the creation of delayed goals, analogous to `Zzz`, and the utility functions `promised`, `future` and `take-now` help with the dotted-lists created by these promises. The miniKanren wrappers `run`, `run*`, `pull`, `take` and `take-all` are appropriately extended.

```
(define $0 (run* (q) (disj (== q 4) (next (== q 5)))))

$0
;; => (4 . #<promise>)

(take-now $0)
;; => (4)

(promised $0)
;; => #<promise>

(take-next $0)
;; => (5)
```

Disjunction is simple: in successive calls to `disj` or `mplus`, promises are shunted right and grouped together in a single promise.

```
;; equivalent

(disj (next (== #(0) 6)) (disj (== #(0) 5) (next (== #(0) 7))))

(disj (== #(0) 5) (next (disj (== #(0) 6) (== #(0) 7))))
```

Conjunction is a little more complicated as soon as we allow for recursive promises. In `(conj g1 g2)`, we want the promises created by g1 and g2 to be forced at the same time, while any recursive promises created by those two groups of promises to be delayed to yet a further time. Simultaneity between two delayed predicates is defined as being nested in an equivalent number of `delay`s.

```
(define $1
  (run* (q)
    (next (== q 4))
    (disj (next (next (== q 5)))
          (next (== q 4)))))

$1
;; => #<promise>

(take-next $1)
;; => (4 . #<promise>)

(take-next (take-next $1))
;; => ()

;; a bit impure...
(define (inco x)
  (let r ((n 0))
    (disj (== x n) (next (r (+ n 1))))))

(define $2
  (run* (q)
    (fresh (a b)
      (== (list a b) q)
      (conj (inco a) (inco b)))))

$2
;; => ((0 0) . #<promise>)

(take-next $2)
;; => ((0 1) (1 0) (1 1) . #<promise>)

(take-next (take-next $2))
;; => ((1 2) (2 0) (2 1) (2 2) (0 2) . #<promise>)

(take-next (take-next (take-next $2)))
;; => ((0 3) (2 3) (3 0) (3 1) (3 2) (3 3) (1 3) . #<promise>)
    
```



## Original License

Copyright (C) 2013 Jason Hemann and Daniel P. Friedman

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



