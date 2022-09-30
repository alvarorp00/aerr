; Script that verifies the swapping problem, after performing the transformation
; of array theory into formulas with uninterpreted functions

(set-logic QF_UF)

(declare-sort Int)
(declare-fun x (Int) Int)
(declare-fun x1 (Int) Int)
(declare-fun xp (Int) Int)
(declare-const i Int)
(declare-const j Int)
(declare-const e Int)
(declare-const k Int)


(assert (= e (x i)))
(assert (= (x1 i) (x j)))
(assert (=> (not (= j i)) (= (x1 j) (x j))))
(assert (=> (not (= k i)) (= (x1 k) (x k))))
(assert (= (xp j) e))
(assert (=> (not (= i j)) (= (xp i) (x1 i))))
(assert (=> (not (= k j)) (= (xp k) (x1 k))))


(assert 
      (or
          (not (= (xp i) (x j)))
          (not (= (xp j) (x i)))
          (not (=> (and (not (= k i)) (not (= k j))) (= (xp k) (x k))))
      ))

(check-sat)
