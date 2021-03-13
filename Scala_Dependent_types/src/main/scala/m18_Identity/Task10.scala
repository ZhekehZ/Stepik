package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task10 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val b1 = "b1" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recBBB = BoolInd.rec(Bool ->: Bool)
  val and = recBBB(b :-> b)(b :-> fls)

  val indB_b_and_tru_eq_b = BoolInd.induc(b :-> (and(b)(tru) =:= b))
  val b_and_tru_eq_b = indB_b_and_tru_eq_b(tru.refl)(fls.refl)  !: b ~>: (and(b)(tru) =:= b)

  val indB_b_and_fls_eq_fls = BoolInd.induc(b :-> (and(b)(fls) =:= fls))
  val b_and_fls_eq_fls = indB_b_and_fls_eq_fls(fls.refl)(fls.refl)  !: b ~>: (and(b)(fls) =:= fls)

  val indB_b_and_b1_eq_b1_and_b = BoolInd.induc(b :-> (b1 ~>: ( and(b)(b1) =:= and(b1)(b) )))
  val b_and_b1_eq_b1_and_b = indB_b_and_b1_eq_b1_and_b(
       b1 :~> IdentityTyp.symm(Bool) (and(b1)(tru)) (b1) (b_and_tru_eq_b(b1))
    )( b1 :~> IdentityTyp.symm(Bool) (and(b1)(fls)) (fls) (b_and_fls_eq_fls(b1)) )

  def main(args: Array[String]): Unit = {
    b_and_b1_eq_b1_and_b !: b ~>: (b1 ~>: ( and(b)(b1) =:= and(b1)(b) ))
  }
}
