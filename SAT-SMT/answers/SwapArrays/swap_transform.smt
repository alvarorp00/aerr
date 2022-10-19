; Script that verifies the swapping problem, after performing the transformation
; of array theory into formulas with uninterpreted functions

(set-logic QF_UF)

(declare-sort Int)
(declare-fun x (Int) Int)
(declare-fun x1 (Int) Int)
(declare-fun xp (Int) Int)
(declare-const n Int)
(declare-const m Int)
(declare-const e Int)
(declare-const k Int)


(assert (= e (x n)))
(assert (= (x1 n) (x m)))
(assert (=> (not (= m n)) (= (x1 m) (x m))))
(assert (=> (not (= k n)) (= (x1 k) (x k))))
(assert (= (xp m) e))
(assert (=> (not (= n m)) (= (xp n) (x1 n))))
(assert (=> (not (= k m)) (= (xp k) (x1 k))))


 (assert 
       (or
           (not (= (xp n) (x m)))
           (not (= (xp m) (x n)))
           (not (=> (and (not (= k n)) (not (= k m))) (= (xp k) (x k))))
       ))

; (assert 
;       (or
;           (not (= (xp n) (x m)))
;           (not (= (xp m) (x n)))
;           (and (not (= k n)) (not (= k m)) (not (= (xp k) (x k))))
;       ))

(check-sat)
