package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task08 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recBBB = BoolInd.rec(Bool ->: Bool)
  val and = recBBB(b :-> b)(b :-> fls)

  val tru_and_b_eq_b = b :~> b.refl

  val fls_and_b_eq_fls = b :~> fls.refl

  val indB_b_and_tru_eq_b = BoolInd.induc(b :-> (and(b)(tru) =:= b))
  val b_and_tru_eq_b = indB_b_and_tru_eq_b(tru.refl)(fls.refl)

  val indB_b_and_fls_eq_fls = BoolInd.induc(b :-> (and(b)(fls) =:= fls))
  val b_and_fls_eq_fls = indB_b_and_fls_eq_fls(fls.refl)(fls.refl)

  //noinspection DuplicatedCode
  def main(args: Array[String]): Unit = {
    tru_and_b_eq_b !: b ~>: (and(tru)(b) =:= b)
    fls_and_b_eq_fls !: b ~>: (and(fls)(b) =:= fls)
    b_and_tru_eq_b !: b ~>: (and(b)(tru) =:= b)
    b_and_fls_eq_fls !: b ~>: (and(b)(fls) =:= fls)
  }
}
