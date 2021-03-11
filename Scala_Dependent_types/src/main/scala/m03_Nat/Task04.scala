package m03_Nat

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
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)
  val recNN = NatInd.rec(Nat)
  val addnm = "add(n)(m)" :: Nat
  val add = m :-> recNN(m)(n :-> (addnm :-> succ(addnm) ))
  val multnm = "mult(n)(m)" :: Nat

  val mult = m :-> recNN(zero)(n :-> (multnm :-> add(multnm)(m)))

  def main(args: Array[String]): Unit = {
    assert(mult(zero)(three) == zero)
    assert(mult(one)(four) == four)
    assert(mult(two)(three) == six)
  }
}
