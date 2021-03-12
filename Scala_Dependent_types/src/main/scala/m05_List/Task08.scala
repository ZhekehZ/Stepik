package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task08 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val recLLL = ListAInd.rec(ListA ->: ListA)
  val acc = "acc" :: ListA
  val revertas = "revert_aux(as,_)" :: ListA ->: ListA

  val revert_aux = recLLL(acc :-> acc)(a :-> (as :-> (revertas :-> (acc :-> revertas(cons(a)(acc)) ))))
  val revert = as :-> revert_aux(as)(nil)

  val list = cons(a)(cons(a1)(cons(a2)(nil)))

  def main(args: Array[String]): Unit = {
    assert(revert(list) == cons(a2)(cons(a1)(cons(a)(nil))))
  }
}
