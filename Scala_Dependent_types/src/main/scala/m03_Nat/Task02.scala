package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task02 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val m = "m" :: Nat
  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)

  val recNN = NatInd.rec(Nat)
  val pred = recNN(zero)(n :-> (m :-> n))

  def main(args: Array[String]): Unit = {
    assert(pred(one) == zero)
    assert(pred(two) == one)
    assert(pred(three) == two)
    assert(pred(four) == three)
  }
}
