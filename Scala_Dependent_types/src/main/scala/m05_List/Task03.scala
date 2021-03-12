package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task03 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val recLB = ListAInd.rec(Bool)

  val isNil = recLB(tru)(a :-> (as :-> (b :-> fls)))

  def main(args: Array[String]): Unit = {
    assert(isNil(nil) == tru)
    assert(isNil(cons(a)(nil)) == fls)
  }
}
