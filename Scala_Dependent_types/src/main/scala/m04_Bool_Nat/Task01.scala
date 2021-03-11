package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task01 {

  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val b = "b" :: Bool

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
  val isZero = recNB(tru)(n :-> (b :-> fls))

  def main(args: Array[String]): Unit = {
    assert(isZero(zero) == tru)
    assert(isZero(one) == fls)
    assert(isZero(five) == fls)
  }
}
