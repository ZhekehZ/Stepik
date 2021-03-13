package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task11 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn ="add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val add_zero_n_eq_n = n :~> n.refl

  val indN_add_n_zero_eq_n = NatInd.induc(n :-> (add(n)(zero) =:= n))
  val hyp = "n + 0 = n" :: (add(n)(zero) =:= n)
  val add_n_zero_eq_n = indN_add_n_zero_eq_n(zero.refl)(n :~> (hyp :-> IdentityTyp.induced(succ)(add(n)(zero))(n)(hyp) ))

  def main(args: Array[String]): Unit = {
    add_zero_n_eq_n !: (n ~>: (add(zero)(n) =:= n))
    add_n_zero_eq_n !: (n ~>: (add(n)(zero) =:= n))
  }
}
