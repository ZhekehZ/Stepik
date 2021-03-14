package m18_Identity

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task03 {
  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val Id = "Id" :: A ~>: (A ->: A ->: Type)
  val IdInd = ("refl" ::: A ~>>: a ~>>: (Id -> Id(A)(a)(a) )) =:: Id
  val refl :: HNil = IdInd.intros
  val a1_eq_a2 = "_ : a1=a2" :: Id(A)(a1)(a2)

  val B = "B" :: Type
  val f = "f" :: A ->: B

  val ind_a1eqa2_fa1eqfa2 = IdInd.induc(A :~> (a1 :~> (a2 :~> (a1_eq_a2 :-> (B ~>: f ~>: Id(B)(f(a1))(f(a2)) )))))
  val map = ind_a1eqa2_fa1eqfa2(A :~> (a :~> (B :~> (f :~> refl(B)(f(a)) ))))

  def main(args: Array[String]): Unit = {
    map !: A ~>: a1 ~>: a2 ~>: (Id(A)(a1)(a2) ->: B ~>: f ~>: Id(B)(f(a1))(f(a2)) )
  }
}
