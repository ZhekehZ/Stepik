package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task05 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val Id = "Id" :: A ~>: (A ->: A ->: Type)
  val IdInd = ("refl" ::: A ~>>: a ~>>: (Id -> Id(A)(a)(a) )) =:: Id
  val refl :: HNil = IdInd.intros
  val a1_eq_a2 = "_ : a1=a2" :: Id(A)(a1)(a2)

  val B = "B(_ : A)" :: A ->: Type
  val b = "b" :: B(a)
  val f = "f" :: a ~>: B(a)

  val ind_a1eqa2_ba1_ba2 = IdInd.induc(A :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (B ~>: (B(a1) ->: B(a2) ))))))
  val transport = ind_a1eqa2_ba1_ba2(A :~> (a :~> (B :~> (b :-> b))))

  val indDepMap = IdInd.induc(A :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (B ~>: f ~>: Id(B(a2))( transport(A)(a1)(a2)(a1_eq_a2)(B)(f(a1)) )(f(a2)) )))))
  val depMap = indDepMap(A :~> (a :~> (B :~> (f :~> refl(B(a))(f(a)) ))))

  def main(args: Array[String]): Unit = {
    depMap !: A ~>: a1 ~>: a2 ~>: a1_eq_a2 ~>: B ~>: f ~>: Id(B(a2))( transport(A)(a1)(a2)(a1_eq_a2)(B)(f(a1)) )(f(a2))
  }
}
