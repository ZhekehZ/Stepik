package m17_Matrices

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task01 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val seven = succ(six)
  val eight = succ(seven)

  val n = "n" :: Nat
  val m = "m" :: Nat

  val a = "a" :: Nat
  val a1 = "a1" :: Nat
  val a2 = "a2" :: Nat
  val a3 = "a3" :: Nat
  val a4 = "a4" :: Nat
  val a5 = "a5" :: Nat
  val a6 = "a6" :: Nat
  val a7 = "a7" :: Nat

  val Vec = "Vec" :: Nat ->: Type
  val VecInd = ("nil" ::: (Vec -> Vec(zero))) |:
               ("cons" ::: n ~>>: (Nat ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))) =:: Vec
  val vnil :: vcons :: HNil = VecInd.intros

  val vn = "v_n" :: Vec(n)
  val vm = "v_m" :: Vec(m)
  val wm = "w_m" :: Vec(m)

  val recNN = NatInd.rec(Nat)
  val errorN = "error" :: Nat
  val pred = recNN(errorN)(n :-> (m :-> n))

  val recVN = VecInd.rec(Nat)
  val errorEl = "error" :: Nat
  val vhead = recVN(errorEl)(n :~> (a :-> (vn :-> (a1 :-> a))))

  val indVnVpn = VecInd.induc(n :~> (vn :-> Vec(pred(n)) ))
  val errorVec = "error" :: Vec(pred(zero))
  val vpn = "v_(pred(n))" :: Vec(pred(n))
  val vtail = indVnVpn(errorVec)(n :~> (a :-> (vn :-> (vpn :-> vn))))

  val Matrix = "Matrix" :: Nat ->: Nat ->: Type
  val MatrixInd = { "nil" ::: m ~>>: (Matrix -> Matrix(zero)(m))} |:
                  { "cons" ::: n ~>>: (m ~>>: (Vec(m) ->>: (Matrix :> Matrix(n)(m)) -->>: (Matrix -> Matrix(succ(n))(m))))} =:: Matrix
  val mnil :: mcons :: HNil = MatrixInd.intros

  val matnm = "mat_nm" :: Matrix(n)(m)

  val indMnmVm = MatrixInd.induc(n :~> (m :~> (matnm :-> Vec(m) )))
  val errorVecm = "error" :: Vec(m)
  val mhead = indMnmVm(m :~> errorVecm)(n :~> (m :~> (vm :-> (matnm :-> (wm :-> vm)))))

  val indMnmMpnm = MatrixInd.induc(n :~> (m :~> (matnm :-> Matrix(pred(n))(m) )))
  val errorMat = "error" :: Matrix(pred(zero))(m)
  val matpnm = "v_(pred(n))" :: Matrix(pred(n))(m)
  val mtail = indMnmMpnm(m :~> errorMat)(n :~> (m :~> (vm :-> (matnm :-> (matpnm :-> matnm)))))

  val indNMn0 = NatInd.induc(n :-> Matrix(n)(zero))
  val matn0 = "mat_n0" :: Matrix(n)(zero)

  val replicateNil = indNMn0(mnil(zero))(n :~> (matn0 :-> mcons(n)(zero)(vnil)(matn0) ))

  val indNnMnmMnsm = NatInd.induc(n :-> (Vec(n) ->: (m ~>: (Matrix(n)(m) ->: Matrix(n)(succ(m))) )))
  val v0 = "v_0" :: Vec(zero)
  val mat0m = "mat_0m" :: Matrix(zero)(m)
  val vsn = "v_(succ(n))" :: Vec(succ(n))
  val matsnm = "mat_(succ(n),m)" :: Matrix(succ(n))(m)
  val zipWithConsn = "zipWithCons(n)" :: Vec(n) ->: (m ~>: (Matrix(n)(m) ->: Matrix(n)(succ(m))) )

  val zipWithCons = indNnMnmMnsm(v0 :-> (m :~> (mat0m :-> mnil(succ(m)) )))(
            n :~> (zipWithConsn :-> (vsn :-> (m :~> (matsnm :->
              mcons (n) (succ(m))
                  (vcons (m) (vhead (succ(n)) (vsn)) (mhead (succ(n)) (m) (matsnm)) )
                  (zipWithConsn (vtail (succ(n)) (vsn)) (m) (mtail (succ(n)) (m) (matsnm)) )
              )))))

  val indMnmMmn = MatrixInd.induc(n :~> (m :~> (matnm :-> Matrix(m)(n) )))
  val matmn = "mat_mn" :: Matrix(m)(n)

  val transpose = indMnmMmn(m :~> replicateNil (m))(
            n :~> (m :~> (vm :-> (matnm :-> (matmn :-> zipWithCons (m) (vm) (n) (matmn))))))

  val vect = vcons(two)(a)(vcons(one)(a1)(vcons(zero)(a2)(vnil))) !: Vec(three)
  val vect1 = vcons(two)(a3)(vcons(one)(a4)(vcons(zero)(a5)(vnil))) !: Vec(three)
  val mat = mcons(one)(three)(vect)(mcons(zero)(three)(vect1)(mnil(three))) !: Matrix(two)(three)
  val vect2 = vcons(one)(a)(vcons(zero)(a3)(vnil)) !: Vec(two)
  val vect3 = vcons(one)(a1)(vcons(zero)(a4)(vnil)) !: Vec(two)
  val vect4 = vcons(one)(a2)(vcons(zero)(a5)(vnil)) !: Vec(two)
  val mat1 = mcons(two)(two)(vect2)(mcons(one)(two)(vect3)(mcons(zero)(two)(vect4)(mnil(two)))) !: Matrix(three)(two)

  def main(args: Array[String]): Unit = {
    replicateNil !: n ~>: Matrix(n)(zero)
    zipWithCons !: n ~>: (Vec(n) ->: (m ~>: (Matrix(n)(m) ->: Matrix(n)(succ(m))) ))
    transpose !: n ~>: (m ~>: (Matrix(n)(m) ->: Matrix(m)(n) ))
    assert(transpose(two)(three)(mat) == mat1)
  }
}
