; Example: arrays as uninterpreted functions

(set-logic QF_UFLIA)

(declare-fun a (Int) Int)
(declare-fun ap (Int) Int)
(declare-const n Int)
(declare-const x Int)

(assert (=> (<= 0 (- n 1)) (= (a 0) 0)))
(assert (=> (<= 0 (- n 1)) (= (a (- n 1)) 0)))
(assert (=> (and (<= 0 x) (<= x (- n 1))) (= (a x) 0)))
(assert (= (ap n) 0))
(assert (=> (or (<= 0 (- n 1)) (<= (+ n 1) 0)) (= (ap 0) (a 0))))
(assert (= (ap (- n 1)) (a (- n 1))))
(assert (= (ap (+ n 1)) (a (+ n 1))))
(assert (=> (or (<= x (- n 1)) (<= (+ n 1) x)) (= (ap x) (a x))))
(assert (and (<= 0 x) (<= x n)))
(assert (not (= (ap x) 0)))
(check-sat)