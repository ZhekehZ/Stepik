package m15_Vector

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val a3 = "a3" :: A
  val a4 = "a4" :: A

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn ="add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val Vec = "Vec" :: Nat ->: Type
  val VecInd = ("nil" ::: (Vec -> Vec(zero))) |:
               ("cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))) =:: Vec
  val vnil :: vcons :: HNil = VecInd.intros
  val vn = "v_n" :: Vec(n)

  val indVAV = VecInd.induc(n :~> (vn :-> (A ->: Vec(succ(n)) )))
  val appendvn = "append(v_n)" :: A ->: Vec(succ(n))

  val append = indVAV(
                  a :-> vcons (zero) (a) (vnil) )(
                  n :~> (a1 :-> (vn :-> (appendvn :-> (a :->
                    vcons (succ(n)) (a1) (appendvn(a)) )))))

  def main(args: Array[String]): Unit = {
    assert(append(two)(vcons(one)(a)(vcons(zero)(a1)(vnil)))(a2) ==
            vcons(two)(a)(vcons(one)(a1)(vcons(zero)(a2)(vnil))))
  }
}
