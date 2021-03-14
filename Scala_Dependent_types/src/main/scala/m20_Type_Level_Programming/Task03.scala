package m20_Type_Level_Programming

import shapeless.{::, HList, HNil, Nat, Poly1, poly}
import shapeless.ops.hlist.{Collect, Prepend}
import shapeless.ops.nat.{GT, LTEq}
import shapeless.nat.{_0, _1, _2, _3, _4, _5, _6, _7, _8, _9}

//noinspection TypeAnnotation
object Task03 {
  trait Sort[L <: HList] {
    type Out <: HList
  }

  implicit val hnilSort = new Sort[HNil] { type Out = HNil }

  implicit def hConsSort[H <: Nat, T <: HList,
    T_lo <: HList, T_hi <: HList,
    T_lo_srt <: HList, T_hi_srt <: HList]
    (implicit
          collectLo: Collect[T, Low[H]] { type Out = T_lo },
          collectHi: Collect[T, High[H]] { type Out = T_hi },
          sortLo: Sort[T_lo] { type Out = T_lo_srt },
          sortHi: Sort[T_hi] { type Out = T_hi_srt },
          prepend: Prepend[T_lo_srt, H :: T_hi_srt]
         ) = new Sort[H :: T] { type Out = prepend.Out }

  trait Low[H <: Nat] extends Poly1
  object Low {
    implicit def `case`[H <: Nat, N <: Nat](implicit lt: LTEq[N, H]) = poly.Case1[Low[H], N, N](n => n)
  }

  trait High[H <: Nat] extends Poly1
  object High {
    implicit def `case`[H <: Nat, N <: Nat](implicit gt: GT[N, H]) = poly.Case1[High[H], N, N](n => n)
  }

  def main(args: Array[String]): Unit = {
    type t = Sort[_1 :: _0 :: _2 :: HNil] { type Out = _0 :: _1 :: _2 :: HNil }
  }
}
