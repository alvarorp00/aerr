; Exercise 6 (c)

(declare-sort Elem)
(declare-fun setA (Elem) Bool)
(declare-fun setB (Elem) Bool)


; A ∩ B = empty
(assert (not (exists ((x Elem)) (and (setA x) (setB x)))))


; Negation of A - B = A
(assert (not (forall ((x Elem)) (= (and (setA x) (not (setB x))) (setA x)))))

(check-sat)