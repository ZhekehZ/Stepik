package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._


//noinspection TypeAnnotation,DuplicatedCode
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

  val recNN = NatInd.rec(Nat)
  val pred = recNN(zero)(n :-> (m :-> n))

  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val b1 = "b1" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val recNB = NatInd.rec(Bool)
  val isZero = recNB(tru)(n :-> (b :-> fls))

  val recBBBB = BoolInd.rec(Bool ->: Bool ->: Bool)
  val ifElse = recBBBB(b :-> (b1 :-> b))(b :-> (b1 :-> b1))

  val recNNB = NatInd.rec(Nat ->: Bool)
  val isEqualn = "isEqual(n)" :: Nat ->: Bool

  val isEqual = recNNB(isZero)(n :-> (isEqualn :-> (m :-> ifElse(isZero(m))(fls)(isEqualn(pred(m))))))

  def main(args: Array[String]): Unit = {
    val nums : Array[Term] = Array(zero, one, two, three, four, five, six)

    for (i <- 0 to 6)
      for (j <- 0 to 6)
        if (i == j)
          assert(isEqual(nums(i))(nums(j)) == tru, "" + i + " == " + j)
        else
          assert(isEqual(nums(i))(nums(j)) == fls, "" + i + " != " + j)
  }
}
