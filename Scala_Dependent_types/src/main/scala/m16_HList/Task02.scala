package m16_HList

object Task02 {
  sealed trait Nat
  case class Succ[N <: Nat](n: N) extends Nat
  case object Zero extends Nat
  type Zero = Zero.type
  type One = Succ[Zero]
  type Two = Succ[One]
  type Three = Succ[Two]

  sealed trait HList {
    type Size <: Nat
  }
  case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    override type Size = Succ[tail.Size]
  }
  case object HNil extends HList {
    override type Size = Zero
  }
  type HNil = HNil.type

//  def main(args: Array[String]): Unit = {
//    implicitly[HCons[Int, HCons[String, HCons[Boolean, HNil]]]# Size =:= Three]
//  }
}
