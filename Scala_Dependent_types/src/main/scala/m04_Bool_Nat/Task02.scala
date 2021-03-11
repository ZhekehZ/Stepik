package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task02 {
  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val b = "b" :: Bool

  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)

  val recNB = NatInd.rec(Bool)
  val isEven = recNB(tru)(n :-> not)
  val isOdd = n :-> not(isEven(n))

  def main(args: Array[String]): Unit = {
    assert(isEven(zero) == tru)
    assert(isEven(two) == tru)
    assert(isEven(four) == tru)

    assert(isEven(one) == fls)
    assert(isEven(three) == fls)
    assert(isEven(five) == fls)

    assert(isOdd(one) == tru)
    assert(isOdd(three) == tru)
    assert(isOdd(five) == tru)

    assert(isOdd(zero) == fls)
    assert(isOdd(two) == fls)
    assert(isOdd(four) == fls)
  }
}
