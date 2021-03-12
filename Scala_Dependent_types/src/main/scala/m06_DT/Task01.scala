package m06_DT

object Task01 {
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

  object Circle extends Shape{
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

  def main(args: Array[String]): Unit = {
    implicitly[Circle# CornerCount =:= Zero]
    implicitly[Triangle# CornerCount =:= Three]
    implicitly[Square# CornerCount =:= Four]
  }
}
