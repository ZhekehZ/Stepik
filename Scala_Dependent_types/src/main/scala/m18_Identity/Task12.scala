package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task12 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("S" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNN = NatInd.rec(Nat)
  val double = recNN(zero)(n :-> (m :-> succ(succ(m)) ))

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val indN_naddSm_eq_S_naddm = NatInd.induc(n :-> (m ~>: ( add(n)(succ(m)) =:= succ(add(n)(m)) )))
  val hyp = "n+Sm=S(n+m)" :: (m ~>: ( add(n)(succ(m)) =:= succ(add(n)(m)) ))
  val naddSm_eq_S_naddm = indN_naddSm_eq_S_naddm(m :~> succ(m).refl)(n :~> (hyp :-> (m :~>
    IdentityTyp.induced( succ )( add(n)(succ(m)) )( succ(add(n)(m)) )( hyp(m) )
    )))

  def main(args: Array[String]): Unit = {
    naddSm_eq_S_naddm !: n ~>: m ~>: ( add(n)(succ(m)) =:= succ(add(n)(m)) )
  }
}
