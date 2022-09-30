(set-logic QF_NIA)

(declare-const r Int)
(declare-const x Int)
(assert (>= x 0))

(assert (= r 0))
(assert (not (<= (* r r) x)))
(check-sat)