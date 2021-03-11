package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task13 {
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
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)

  val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
  val m1 = "half(n)" :: Nat
  val m2 = "half(n+1)" :: Nat
  val halfn = "half_aux(n,_,_)" :: Nat ->: Nat ->: Nat

  val half_aux = recNNNN(m1 :-> (m2 :-> m1))(n :-> (halfn :-> (m1 :-> (m2 :-> halfn(m2)(succ(m1)) ))))

  val half = n :-> half_aux(n)(zero)(zero)

  def main(args: Array[String]): Unit = {
    assert(half(three) == one)
    assert(half(two) == one)
    assert(half(nine) == four)
  }
}
