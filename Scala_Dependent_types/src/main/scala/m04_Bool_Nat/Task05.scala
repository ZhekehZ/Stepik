package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task05 {
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

  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros
  val b = "b" :: Bool

  val recNB = NatInd.rec(Bool)
  val isZero = recNB(tru)(n :-> (b :-> fls))

  val recNN = NatInd.rec(Nat)
  val pred = recNN(zero)(n :-> (m :-> n))

  val recBNNN = BoolInd.rec(Nat ->: Nat ->: Nat)
  val ifElse = recBNNN(n :-> (m :-> n))(n :-> (m :-> m))

  val recNNN = NatInd.rec(Nat ->: Nat)
  val subtractn = "subtract(_,n)" :: Nat ->: Nat
  val subtract = n :-> (m :-> recNNN(m :-> m)(n :-> (subtractn :-> (m :-> subtractn(pred(m))))) (m)(n))

  def main(args: Array[String]): Unit = {
    val nums : Array[Term] = Array(zero, one, two, three, four, five, six)

    for (i <- 0 to 6)
      for (j <- i to 6)
        assert(subtract(nums(j))(nums(i)) == nums(j - i), "" + j + " - " + i + " == " + (j - i))
  }
}
