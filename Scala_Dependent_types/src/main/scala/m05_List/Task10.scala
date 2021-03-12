package m05_List

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task10 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val ListA = "List(A)" :: Type
  val as = "as" :: ListA
  val as1 = "as1" :: ListA
  val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  val Nat = "Nat" :: Type
  val n = "n" :: Nat
  val m = "m" :: Nat
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)

  val recNN = NatInd.rec(Nat)
  val pred = recNN(zero)(n :-> (m :-> n))

  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recNB = NatInd.rec(Bool)
  val isZero = recNB(tru)(n :-> (b :-> fls))
  val recLNL = ListAInd.rec(Nat ->: ListA)
  val recBLLL = BoolInd.rec(ListA ->: ListA ->: ListA)
  val ifElse = recBLLL(as :-> (as1 :-> as))(as :-> (as1 :-> as1))
  val takeas = "take(as)" :: Nat ->: ListA

  val take = recLNL(n :-> nil)(a :-> (as :-> (takeas :-> (n :->
    ifElse(isZero(n))(nil)(cons(a)(takeas(pred(n)))) ))))

  var list = cons(a)(cons(a1)(cons(a2)(nil)))

  def main(args: Array[String]): Unit = {
    assert(take(list)(two) == cons(a)(cons(a1)(nil)))
    assert(take(list)(one) == cons(a)(nil))
    assert(take(list)(zero) == nil)
    assert(take(list)(four) == list)
    assert(take(list)(three) == list)
  }
}
