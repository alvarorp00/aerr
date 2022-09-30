(set-logic QF_NIA)

(declare-const r Int)
(declare-const rp Int)
(declare-const x Int)
(assert (>= x 0))

(assert (<= (* r r) x))
(assert (<= (* (+ r 1) (+ r 1)) x))
(assert (= rp (+ r 1)))

(assert (not (<= (* rp rp) x)))
(check-sat)