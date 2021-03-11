package m01_BooleanType

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task04 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recBBB = BoolInd.rec(Bool ->: Bool)

  val or = recBBB(b :-> tru)(b :-> b)

  def main(args: Array[String]): Unit = {
    assert(or(tru)(tru) == tru)
    assert(or(tru)(fls) == tru)
    assert(or(fls)(tru) == tru)
    assert(or(fls)(fls) == fls)
  }
}
