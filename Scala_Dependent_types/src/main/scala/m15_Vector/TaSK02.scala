package m15_Vector

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object TaSK02 {
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
  val vm = "v_m" :: Vec(m)

  val indVVV = VecInd.induc(n :~> (vn :-> (m ~>: (Vec(m) ->: Vec(add(n)(m)) ))))
  val concatVn = "concat(v_n)" :: (m ~>: (Vec(m) ->: Vec(add(n)(m)) ))

  val vconcat = indVVV(
                  m :~> (vm :-> vm)  )(
                  n :~> (a :-> (vn :-> (concatVn :-> (m :~> (vm :->
                    vcons (add(n)(m)) (a) (concatVn (m) (vm)) ))))))

  def main(args: Array[String]): Unit = {
    val vect = vcons(one)(a)(vcons(zero)(a1)(vnil))
    val vect1 = vcons(two)(a2)(vcons(one)(a3)(vcons(zero)(a4)(vnil)))
    assert(vconcat(two)(vect)(three)(vect1) == vcons(four)(a)(vcons(three)(a1)(vcons(two)(a2)(vcons(one)(a3)(vcons(zero)(a4)(vnil))))))
  }
}
