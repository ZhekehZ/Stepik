package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
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
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)

  val recNN = NatInd.rec(Nat)
  val triple = recNN(zero)(n :-> (m :-> succ(succ(succ(m)))))

  def main(args: Array[String]): Unit = {
    assert(triple(zero) == zero)
    assert(triple(one) == three)
    assert(triple(two) == six)
    assert(triple(three) == nine)
  }
}
