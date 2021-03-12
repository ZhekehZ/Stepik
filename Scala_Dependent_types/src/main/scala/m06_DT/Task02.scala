package m06_DT

//noinspection TypeAnnotation
object Task02 {
  sealed trait Nat
  class Succ[N <: Nat] extends Nat
  object Zero extends Nat
  type Zero = Zero.type
  type One = Succ[Zero]
  type Two = Succ[One]
  type Three = Succ[Two]
  type Four = Succ[Three]

  sealed trait Shape {
    type CornerCount <: Nat
  }

  object Circle extends Shape {
    type CornerCount = Zero
  }
  type Circle = Circle.type

  object Triangle extends Shape {
    type CornerCount = Three
  }
  type Triangle = Triangle.type

  object Square extends Shape {
    type CornerCount = Four
  }
  type Square = Square.type

  trait CornerCount[In <: Shape] {
    type Out <: Nat
  }

  implicit val circleCornerCount = new CornerCount[Circle] {
    override type Out = Zero
  }
  implicit val triangleCornerCount =  new CornerCount[Triangle] {
    override type Out = Three
  }
  implicit val squareCornerCount = new CornerCount[Square] {
    override type Out = Four
  }

  def main(args: Array[String]): Unit = {
    implicitly[CornerCount[Circle] { type Out = Zero } ]
    implicitly[CornerCount[Triangle] { type Out = Three } ]
    implicitly[CornerCount[Square] { type Out = Four } ]
  }
}
