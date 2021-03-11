package m04_Bool_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task04 {
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

  val recBB = BoolInd.rec(Bool)
  val not = recBB(fls)(tru)

  val recNB = NatInd.rec(Bool)
  val isZero = recNB(tru)(n :-> (b :-> fls))

  val recBBBB = BoolInd.rec(Bool ->: Bool ->: Bool)
  val ifElse = recBBBB(b :-> (b1 :-> b))(b :-> (b1 :-> b1))

  val recNNB = NatInd.rec(Nat ->: Bool)
  val isLessn = "isLess(n)" :: Nat ->: Bool
  val isLess = recNNB(m :-> not(isZero(m))) (n :-> (isLessn :-> (m :-> isLessn(pred(m)))))
  val isGreater = n :-> (m :-> isLess(m)(n))

  def main(args: Array[String]): Unit = {
    val nums : Array[Term] = Array(zero, one, two, three, four, five, six)

    for (i <- 0 to 6)
      for (j <- i + 1 to 6) {
        assert(isLess(nums(i))(nums(j)) == tru, "" + i + " < " + j)
        assert(isGreater(nums(j))(nums(i)) == tru, "" + j + " > " + i)
        assert(isLess(nums(j))(nums(i)) == fls, "" + j + " !< " + i)
        assert(isGreater(nums(i))(nums(j)) == fls, "" + i + " !> " + j)
      }
  }
}
