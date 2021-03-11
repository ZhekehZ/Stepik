package m03_Nat

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task11 {
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

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))

  val multn = "mult(n)" :: Nat ->: Nat
  val mult = recNNN(m :-> zero)(n :-> (multn :-> (m :-> add(multn(m))(m) )))

  val acc = "acc" :: Nat
  val factn = "fact_aux(n,_)" :: Nat ->: Nat
  val fact_aux = recNNN(acc :-> acc)(n :-> (factn :-> (acc :-> factn(mult(succ(n))(acc)) )))
  val fact = n :-> fact_aux(n)(one)

  def main(args: Array[String]): Unit = {
    assert(fact(one) == one)
    assert(fact(two) == two)
    assert(fact(three) == six)
    assert(fact(zero) == one)
  }
}
