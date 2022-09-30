; Example: linear real arithmetic (section 3)

(set-logic QF_LRA)

(declare-const x Real)
(declare-const y Real)
(assert (<= y (- 2 (* 3x))))
(assert (> x 0))
(assert (>= y 0))

(check-sat)
(get-value (x y))