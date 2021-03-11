package m02_DSL

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,ScalaUnusedSymbol
object Tasks {
  def task01() {
    val Bool = "Boolean" :: Type
    val b = "b" :: Bool
  }

  def task02() {
    val A = "A" :: Type

    val C = "C(_ : U0)" :: Type ->: Type
    val c = "c" :: C(A)

    val List = "List(_: I)" :: Type ->: Type
    val l = "l" :: List(A)
  }

  def task03() {
    val Nat = "Nat" :: Type
    val n = "n" :: Nat

    val VecA = "Vec(_ : Nat)" :: Nat ->: Type
    val vn = "vn" :: VecA(n)
  }

  def task04() {
    val A = "A" :: Type
    val B = "B" :: Type
    val F = "F(_ : U0)" :: Type ->: Type
    val fa = "fa" :: F(A)

    val mapF = "map(F)" :: A ~>: B ~>: ((A ->: B) ->: F(A) ->: F(B))
    val f = "f" :: A ->: B

    val T = F ~>: (A ~>: (F(A) ->: (B ~>: ((A ->: B) ->: F(B)))))

    F :~> (A :~> (fa :-> (B :~> (f :-> mapF(A)(B)(f)(fa))))) !: T
  }

  def task05() {
    val A = "A" :: Type
    val F = "F(_ : U0)" :: Type ->: Type
    val fa = "fa" :: F(A)

    val Bool = "Boolean" :: Type
    val filterF = "filter(F)" :: A ~>: ((A ->: Bool) ->: F(A) ->: F(A))
    val p = "p" :: A ->: Bool
    val g = A :~> (F :~> (fa :-> (p :-> filterF(A)(p)(fa))))
    g !: A ~>: F ~>: (F(A) ->: (A ->: Bool) ->: F(A))
  }

  def task06() {
    val Nat = "Nat" :: Type
    val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros

    val Int = "Integer" :: Type
    val IntInd = ("pos" ::: Nat ->>: Int) |: ("neg" ::: Nat ->>: Int) =: Int
    val pos :: neg :: HNil = IntInd.intros

    val Expr = "Expression" :: Type
    val ExprInd = ("number" ::: Int ->>: Expr) |:
      ("negate" ::: Expr -->>: Expr) |:
      ("add" ::: Expr -->>: Expr -->>: Expr) |:
      ("mult" ::: Expr -->>: Expr -->>: Expr) =:
        Expr
    val number :: negate :: add :: mult :: HNil = ExprInd.intros
  }

  def task07() {
    val A = "A" :: Type
    val List = "List" :: Type ->: Type

    val ListInd = ("nil" ::: A ~>>: (List -> List(A))) |:
      ("cons" ::: A ~>>: (A ->>: (List :> List(A)) -->>: (List -> List(A)))) =:: List

    val nil :: cons :: HNil = ListInd.intros
  }

  def main(args: Array[String]): Unit = {
    task01()
    task02()
    task03()
    task04()
    task05()
    task06()
    task07()
  }
}
