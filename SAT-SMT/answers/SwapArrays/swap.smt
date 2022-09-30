; Script that verifies the swapping problem, using Z3 array theory

(set-logic QF_ALIA)

(declare-const x (Array Int Int))
(declare-const x1 (Array Int Int))
(declare-const xp (Array Int Int))
(declare-const i Int)
(declare-const j Int)
(declare-const e Int)

(assert (= e (select x i)))
(assert (= x1 (store x i (select x j))))
(assert (= xp (store x1 j e)))


(assert (not 
      (and
          (= (select xp i) (select x j))
          (= (select xp j) (select x i))
          (forall ((k Int)) (=> (and (not (= k i)) (not (= k j))) (= (select xp k) (select x k))))
      )))

(check-sat)