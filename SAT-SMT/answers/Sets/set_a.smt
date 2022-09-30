; Exercise 6 (a)

(declare-sort Elem)
(declare-fun setA (Elem) Bool)
(declare-fun setB (Elem) Bool)
(declare-fun setC (Elem) Bool)
(declare-fun setD (Elem) Bool)

; A ∩ B != empty
(assert (exists ((x Elem)) (and (setA x) (setB x))))

; A <= D
(assert (forall ((x Elem)) (=> (setA x) (setD x))))

; B <= C
(assert (forall ((x Elem)) (=> (setB x) (setC x))))

; Negation of C ∩ D != empty 
(assert (not (exists ((x Elem)) (and (setC x) (setD x)))))

(check-sat)