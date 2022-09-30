; Example: Bit vectors

(set-logic QF_BV)

(declare-const x (_ BitVec 64))
(declare-const y (_ BitVec 64))
(assert (not (= (bvugt (bvsub x y) (_ bv0 64)) (bvugt x y))))

(check-sat)
(get-model)

