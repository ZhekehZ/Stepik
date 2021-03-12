package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros
  val recLA = ListAInd.rec(A)
  val errorEl = "error" :: A

  val head = recLA(errorEl)(a :-> (as :-> (a1 :-> a)))

  val list1 = cons(a)(cons(a1)(cons(a2)(nil)))
  val list2 = cons(a1)(cons(a1)(cons(a2)(nil)))

  def main(args: Array[String]): Unit = {
    assert(head(list1) == a)
    assert(head(list2) == a1)
    assert(head(nil) == errorEl)
  }
}
