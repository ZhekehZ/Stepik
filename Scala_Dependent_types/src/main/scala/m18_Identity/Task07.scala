package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task07 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)

  val indB_not_not_b_eq_b = BoolInd.induc(b :-> (not(not(b)) =:= b))
  val not_not_b_eq_b = indB_not_not_b_eq_b( tru.refl  )( fls.refl )

  def main(args: Array[String]): Unit = {
    not_not_b_eq_b !: b ~>: (not(not(b)) =:= b)
  }
}
