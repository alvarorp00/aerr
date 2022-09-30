; Exercise shown in Section 3

(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)
(declare-const s Bool)
(declare-const v Bool)
(assert (=> p (or q r)))
(assert (=> (and q s) v))
(assert (=> r (or (not p) q)))
(assert (not (=> (and p s) v)))
(check-sat)
