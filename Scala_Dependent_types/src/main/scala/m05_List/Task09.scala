package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task09 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val a3 = "a3" :: A
  val a4 = "a4" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val as1 = "as1" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val recLLL = ListAInd.rec(ListA ->: ListA)
  val concatas = "concat(as)" :: ListA ->: ListA

  val concat = recLLL(as1 :-> as1)(a :-> (as :-> (concatas :-> (as1 :-> cons(a)(concatas(as1)) ))))

  val list = cons(a)(cons(a1)(cons(a2)(nil)))
  val list1 = cons(a3)(cons(a4)(nil))

  def main(args: Array[String]): Unit = {
    assert(concat(list)(list1) == cons(a)(cons(a1)(cons(a2)(cons(a3)(cons(a4)(nil))))))
    assert(concat(list)(nil) == list)
  }
}
