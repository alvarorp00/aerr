; Example shown in section 3

(set-logic QF_UF)

(declare-sort List 1)
(declare-sort IndexType 0)
(declare-sort ElementType 0)
(declare-const a (List ElementType))
(declare-const b (List ElementType))
(declare-const i IndexType)
(declare-const k ElementType)
(declare-fun get ((List ElementType) IndexType) ElementType)

(assert (= a b))
(assert (= (get a i) k))
(assert (not (= (get b i) k)))
(check-sat)

