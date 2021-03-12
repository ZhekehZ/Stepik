package m08_Product

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
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

  val recNProdNN = NatInd.rec(ProdTyp(Nat, Nat))
  val pair = "(half(n), half(n+1))" :: ProdTyp(Nat, Nat)
  val halfpair = recNProdNN(PairTerm(zero, zero))(n :-> (pair :-> PairTerm(pair.second, succ(pair.first)) ))
  val half = n :-> halfpair(n).first

  def main(args: Array[String]): Unit = {
    assert(half(three) == one)
  }
}
