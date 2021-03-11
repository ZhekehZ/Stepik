package m01_BooleanType

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task03 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)

  val isEqual = b :-> recBB(b)(not(b))

  def main(args: Array[String]): Unit = {
    assert(isEqual(tru)(tru) == tru)
    assert(isEqual(tru)(fls) == fls)
    assert(isEqual(fls)(tru) == fls)
    assert(isEqual(fls)(fls) == tru)
  }
}
