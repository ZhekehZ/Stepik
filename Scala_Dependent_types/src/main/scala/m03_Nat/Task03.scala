package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task03 {
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

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val recNN = NatInd.rec(Nat)
  val double = recNN(zero)(n :-> (m :-> succ(succ(m)) ))
  val square = recNN(zero)(n :-> (m :-> succ(add(m)(double(n))) ))

  def main(args: Array[String]): Unit = {
    assert(square(zero) == zero)
    assert(square(one) == one)
    assert(square(two) == four)
    assert(square(three) == nine)
  }
}
