package m01_Bool

import provingground.HoTT._
import provingground.induction.TLImplicits._
import shapeless._

//noinspection TypeAnnotation,DuplicatedCode
object Task07 {
  val Bool = "Boolean" :: Type
  val b = "b" :: Bool
  val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  val C = "C" :: Type
  val c1 = "c1" :: C
  val c2 = "c2" :: C

  val A = "A" :: Type
  val a = "a" :: A
  val Id = "Id" :: A ~>: (A ->: A ->: Type)
  val IdInd = ("refl" ::: A ~>>: a ~>>: (Id -> Id(A)(a)(a))) =:: Id
  val refl :: HNil = IdInd.intros

  val rec = "rec" :: C ~>: (C ->: C ->: Bool ->: C)
  val beta1 = "rec(c1)(c2)(true)=???" :: C ~>: c1 ~>: c2 ~>:
    Id(C)(rec(C)(c1)(c2)(tru))(c1)
  val beta2 = "rec(c1)(c2)(false)=???" :: C ~>: c1 ~>: c2 ~>:
    Id(C)(rec(C)(c1)(c2)(fls))(c2)

  val recBB = rec(Bool)

  val not = recBB(fls)(tru)
  val and = b :-> recBB(b)(fls)

  def main(args: Array[String]): Unit = {
    beta1(Bool)(fls)(tru) !: Id(Bool)(not(tru))(fls)
    beta2(Bool)(fls)(tru) !: Id(Bool)(not(fls))(tru)
    beta1(Bool)(tru)(fls) !: Id(Bool)(and(tru)(tru))(tru)
    beta1(Bool)(fls)(fls) !: Id(Bool)(and(fls)(tru))(fls)
    beta2(Bool)(tru)(fls) !: Id(Bool)(and(tru)(fls))(fls)
    beta2(Bool)(fls)(fls) !: Id(Bool)(and(fls)(fls))(fls)
  }

}
