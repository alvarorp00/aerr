; Example: pointer logic

(set-logic QF_ALIA)

(declare-sort Var 0)

(declare-const H0 (Array Int Int))
(declare-const H1 (Array Int Int))
(declare-const H2 (Array Int Int))
(declare-const H3 (Array Int Int))


(declare-const L0 (Array Var Int))
(declare-const L1 (Array Var Int))
(declare-const L2 (Array Var Int))
(declare-const L3 (Array Var Int))

(declare-const x Var)
(declare-const y Var)
(declare-const p1 Int)
(declare-const p2 Int)

(assert (not (= x y)))
(assert (not (= p1 p2)))

(assert (= H1 (store H0 p1 5)))
(assert (= L1 (store L0 x p1)))

(assert (= H2 (store H1 p2 (select L1 x))))
(assert (= L2 (store L1 y p2)))

(assert (= H3 (store H2 (select H2 (select L2 y)) 3)))
(assert (= L3 L2))

(assert (not (= (select H3 (select L3 x)) 3)))

(check-sat)

