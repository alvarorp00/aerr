(declare-const p1 Bool)
(declare-const p2 Bool)
(declare-const p3 Bool)
(declare-const p4 Bool)
(declare-const p5 Bool)
(declare-const p6 Bool)

(assert (not p5))
(assert (or p5 (not p6)))
(assert (or p1 p2))
(assert (or (not p1) p3))
(assert (or (not p3) p2))
(assert (or (not p2) p4))
(assert (or (not p4) (not p2)))

(check-sat) ; unsat
; (get-model)