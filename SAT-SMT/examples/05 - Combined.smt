; Example: combination of theories

(set-logic QF_UFLRA)

(declare-const x Real)
(declare-const y Real)
(declare-const z Real)
(declare-fun f (Real) Real)

(assert (<= (f x) y))
(assert (>= (f z) y))
(assert (>= x z))
(assert (>= z x))
(assert (not (= (- y (f x)) 0)))

(check-sat)