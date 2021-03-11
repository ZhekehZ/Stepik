package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task09 {
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

  val recNNN = NatInd.rec(Nat ->: Nat)
  val multn = "mult(n)" :: Nat ->: Nat
  val mult = recNNN(m :-> zero)(n :-> (multn :-> (m :-> add(multn(m))(m) )))

  val pownm = "pow(n)(m)" :: Nat
  val pow = m :-> recNN(one)(n :-> (pownm :-> mult(m)(pownm) ))

  def main(args: Array[String]): Unit = {
    assert(pow(two)(three) == eight)
    assert(pow(zero)(three) == zero)
    assert(pow(one)(three) == one)
    assert(pow(three)(two) == nine)
  }
}
