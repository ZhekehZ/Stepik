package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task05 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val as1 = "as1" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val errorList = "error" :: ListA

  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recBLLL = BoolInd.rec(ListA ->: ListA ->: ListA)
  val ifElse = recBLLL(as :-> (as1 :-> as))(as :-> (as1 :-> as1))

  val recLB = ListAInd.rec(Bool)
  val isNil = recLB(tru)(a :-> (as :-> (b :-> fls)))

  val recLL = ListAInd.rec(ListA)
  val init = recLL(errorList)(a :-> (as :-> (as1 :-> ifElse(isNil(as))(nil)(cons(a)(as1)) )))

  val list = cons(a)(cons(a1)(cons(a2)(nil)))

  def main(args: Array[String]): Unit = {
    assert(init(list) == cons(a)(cons(a1)(nil)))
    assert(init(init(list)) == cons(a)(nil))
    assert(init(init(init(list))) == nil)
    assert(init(init(init(init(list)))) == errorList)
  }
}
