(set-logic QF_NIA)

(declare-const r Int)
(declare-const x Int)
(assert (>= x 0))

(assert (<= (* r r) x))
(assert (not (<= (* (+ r 1) (+ r 1)) x)))

(assert (not (and (<= (* r r) x) (< x (* (+ r 1) (+ r 1))))))
(check-sat)