package m01_Bool

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task05 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)
  val recBBB = BoolInd.rec(Bool ->: Bool)

  val xor = recBBB(b :-> not(b))(b :-> b)

  def main(args: Array[String]): Unit = {
    assert(xor(tru)(tru) == fls)
    assert(xor(tru)(fls) == tru)
    assert(xor(fls)(tru) == tru)
    assert(xor(fls)(fls) == fls)
  }
}
