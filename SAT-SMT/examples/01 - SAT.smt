
; Example shown in slides
; -----------------------
(declare-const p1 Bool)
(declare-const p2 Bool)
(declare-const p3 Bool)
(declare-const p4 Bool)
(declare-const p5 Bool)
(declare-const p6 Bool)
(declare-const p7 Bool)
(declare-const p8 Bool)

(assert (or (not p1) (not p2)))
(assert (or (not p1) p3))
(assert (or (not p4) (not p3) (not p5)))
(assert (or p2 p5 p6))
(assert (or p5 (not p7)))
(assert (or (not p6) p7))
(assert (or (not p5) (not p8) (not p3)))

(check-sat)
(get-model)
;(get-value (p1 p2))
;(get-model)
