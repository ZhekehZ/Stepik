package m14_Pi

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation
object Task01 {
  val Bool = "Boolean" :: Type
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val one = succ(zero)
  val two = succ(one)

  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val recAAA = BoolInd.rec(A ~>: (A ->: A ->: A))

  val ifElse = recAAA(A :~> (a :-> (a1 :-> a)))(A :~> (a :-> (a1 :-> a1)))

  def main(args: Array[String]): Unit = {
    assert(ifElse(fls)(Nat)(one)(two) == two)
    assert(ifElse(tru)(Nat)(one)(two) == one)
  }
}
