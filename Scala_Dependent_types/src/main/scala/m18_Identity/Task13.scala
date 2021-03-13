package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task13 {
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

  val indN_naddn_eq_2n = NatInd.induc(n :-> ( add(n)(n) =:= double(n) ))
  val hyp1 = "n+n=2*n" :: ( add(n)(n) =:= double(n) )
  val lemma1 = IdentityTyp.induced(succ)(add(n)(succ(n)))(succ(add(n)(n)))(naddSm_eq_S_naddm(n)(n))
  val lemma2 = IdentityTyp.induced(n :-> succ(succ(n)))(add(n)(n))(double(n))(hyp1)
  val naddn_eq_2n = indN_naddn_eq_2n(zero.refl)(n :~> (hyp1 :->
    IdentityTyp.trans(Nat)(succ(add(n)(succ(n))))(succ(succ(add(n)(n))))(double(succ(n)))(lemma1)(lemma2)))

  def main(args: Array[String]): Unit = {
    lemma1 !: ( succ(add(n)(succ(n))) =:= succ(succ(add(n)(n))) )
    lemma2 !: ( succ(succ(add(n)(n))) =:= succ(succ(double(n))) )
    naddn_eq_2n !: n ~>: ( add(n)(n) =:= double(n) )
  }
}
