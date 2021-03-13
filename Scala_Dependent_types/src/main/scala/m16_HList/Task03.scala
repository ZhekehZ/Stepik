package m16_HList

//noinspection TypeAnnotation
object Task03 {
  sealed trait Nat
  case class Succ[N <: Nat](n: N) extends Nat
  case object Zero extends Nat
  type Zero = Zero.type
  type One = Succ[Zero]
  type Two = Succ[One]
  type Three = Succ[Two]

  sealed trait HList
  case class HCons[H, T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
  type HNil = HNil.type

  trait Size[L <: HList] {
    type Out <: Nat
  }

  implicit val hNilSize = new Size[HNil] { type Out = Zero }

  implicit def hConsSize[H, T <: HList, N <: Nat] (implicit tailSize: Size[T] { type Out = N } ):
          Size[HCons[H, T]] { type Out = Succ[N] } = new Size[HCons[H, T]] { type Out = Succ[N] }

  def main(args: Array[String]): Unit = {
    implicitly[Size[HCons[Int, HCons[String, HCons[Boolean, HNil]]]] { type Out = Three }]
  }
}
