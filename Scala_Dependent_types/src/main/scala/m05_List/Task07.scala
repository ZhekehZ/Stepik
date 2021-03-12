package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task07 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val as1 = "as1" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val recLAL = ListAInd.rec(A ->: ListA)
  val appendas = "append(as)" :: A ->: ListA
  val append = recLAL(a1 :-> cons(a1)(nil))(a :-> (as :-> (appendas :-> (a1 :-> cons(a)(appendas(a1)) ))))

  val recLL = ListAInd.rec(ListA)

  val revert = recLL(nil)(a :-> (as :-> (as1 :-> append(as1)(a) )))

  val list = cons(a)(cons(a1)(cons(a2)(nil)))

  def main(args: Array[String]): Unit = {
    assert(revert(list) == cons(a2)(cons(a1)(cons(a)(nil))))
  }
}
