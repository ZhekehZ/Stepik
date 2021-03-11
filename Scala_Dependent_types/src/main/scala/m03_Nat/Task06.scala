package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task06 {
  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n = "n" :: Nat
  val m = "m" :: Nat
  val k = "k" :: Nat

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

  val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
  val add3n = "add3(n)" :: Nat ->: Nat ->: Nat

  val add3 = recNNNN(m :-> (k :-> add(m)(k)))(n :-> (add3n :-> (m :-> (k :-> succ(add3n(m)(k))))))

  def main(args: Array[String]): Unit = {
    assert(add3(one)(two)(three) == six)
    assert(add3(four)(five)(zero) == nine)
  }
}
