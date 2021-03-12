package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task02 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val as1 = "as1" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val recLL = ListAInd.rec(ListA)
  val errorList = "error" :: ListA

  val tail = recLL(errorList)(a :-> (as :-> (as1 :-> as)))

  val list = cons(a)(cons(a1)(cons(a2)(nil)))

  val list1 = cons(a1)(cons(a2)(nil))
  val list2 = cons(a2)(nil)

  def main(args: Array[String]): Unit = {
    assert(tail(list) == list1)
    assert(tail(tail(list)) == list2)
    assert(tail(tail(tail(list))) == nil)
    assert(tail(tail(tail(tail(list)))) == errorList)
  }
}
