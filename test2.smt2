(set-logic ALL)
(set-option :produce-models true)
(set-option :pp.decimal true)
(set-option :produce-unsat-cores true)
(declare-const sqrt2 Real)
(assert (= (* sqrt2 sqrt2) 2))
(assert (> sqrt2 0))
(declare-const mem-amp0 (Array Int Int))
(declare-const mem-phase0 (Array Int Int))
(declare-const mem-bit-vec0 (Array Int (_ BitVec 2)))
(declare-const mem-amp1 (Array Int (Array Int Int)))
(declare-const mem-phase1 (Array Int (Array Int Int)))
(declare-const mem-bit-vec1 (Array Int (Array Int (_ BitVec 2))))

;; Uncommenting this gives an 'unknown' result from z3
; (assert (forall ((i Int) (j Int))
;   (=> (and (>= i 0) (>= j 0) (< i 4) (< j 2))
;       (and (= (select (select mem-amp1 j) i)
;               (select mem-amp0 i))
;            (= (select (select mem-phase1 j) i)
;               (sin (* (* 2 pi) (/ (* (bv2nat ((_ extract 0 0) (select mem-bit-vec0 i))) j) 2))))
;            (= (select (select mem-bit-vec1 j) i)
;               (bvor (bvand (select mem-bit-vec0 i) #b11) ((_ int2bv 2) (bv2nat (concat ((_ int2bv 1) j) #b0)))))))))

(declare-const mem-amp2 (Array Int (Array Int Int)))
(declare-const mem-phase2 (Array Int (Array Int Int)))
(declare-const mem-bit-vec2 (Array Int (Array Int (_ BitVec 2))))

(assert (forall ((i Int) (j Int))
  (=> (and (>= i 0) (>= j 0) (< i 4) (< j 2))
      (ite (and (not (= (bv2nat ((_ extract 1 0) ((_ int2bv 2) (bv2nat (select (select mem-bit-vec1 j) i))))) 0)) true)
           (and (= (select (select mem-amp2 j) i) (select (select mem-amp1 j) i))
                (= (select (select mem-phase2 j) i) (select (select mem-phase1 j) i))
                (= (select (select mem-bit-vec2 j) i) ((_ int2bv 2) (mod (+ (bv2nat (select (select mem-bit-vec1 j) i)) 1) 2))))
           (and (= (select (select mem-amp2 j) i) (select (select mem-amp1 j) i))
                (= (select (select mem-phase2 j) i) (select (select mem-phase1 j) i))
                (= (select (select mem-bit-vec2 j) i) (select (select mem-bit-vec1 j) i)))))))

(assert (exists ((i Int)) (= (select mem-bit-vec0 i) ((_ int2bv 2) 0))))
(assert (exists ((i Int)) (= (select mem-bit-vec0 i) ((_ int2bv 2) 0))))
(check-sat)
(get-model)
(get-info :reason-unknown)
(check-sat)
(get-unsat-core)
