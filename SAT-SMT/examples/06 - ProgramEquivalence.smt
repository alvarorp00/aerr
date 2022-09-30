; Example: program equivalence (section 4)

(set-logic QF_UFLIA)

(declare-const x Int)
(declare-const y1 Int)
(declare-const y2 Int)
(declare-const res Int)
(declare-const res_p Int)
(declare-fun f (Int) Int)
(declare-fun g (Int) Int)
(declare-fun h (Int) Int)

(assert (=> (>= x 0) (= y1 (f x))))
(assert (=> (not (>= x 0)) (= y1 (g x))))
(assert (=> (>= x 1) (= y2 (h y1))))
(assert (=> (not (>= x 1)) (= y2 y1)))
(assert (= res y2))

(assert (=> (>= x 1) (= res_p (h (f x)))))
(assert (=> (and (not (>= x 1)) (>= x 0)) (= res_p (f x))))
(assert (=> (and (not (>= x 1)) (not (>= x 0))) (= res_p (g x))))

(assert (not (= res res_p)))

(check-sat)