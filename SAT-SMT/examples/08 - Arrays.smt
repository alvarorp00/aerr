; Example: array logic

(set-logic QF_ALIA)

(declare-const a (Array Int Int))
(declare-const ap (Array Int Int))
(declare-const n Int)

(assert (forall ((i Int)) (=> (and (<= 0 i) (< i n)) (= (select a i) 0))))
(assert (= ap (store a n 0)))
(assert (not (forall ((i Int)) (=> (and (<= 0 i) (<= i n)) (= (select ap i) 0)))))
(check-sat)