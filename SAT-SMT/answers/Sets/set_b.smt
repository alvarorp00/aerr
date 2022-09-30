; Exercise 6 (b)

(declare-sort Elem)
(declare-fun setA (Elem) Bool)
(declare-fun setB (Elem) Bool)


; B <= A
(assert (forall ((x Elem)) (=> (setB x) (setA x))))


; Negation of B ∪ A <= A
(assert (not (forall ((x Elem)) (=> (or (setB x) (setA x)) (setA x)))))

(check-sat)