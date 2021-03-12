package m09_Sum

import provingground.HoTT._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val B = "B" :: Type
  val C = "C" :: Type
  val a = "a" :: A
  val b = "b" :: B
  val c = "c" :: C
  val ab = "ab" :: PlusTyp(A, B)
  val ac = "ac" :: ProdTyp(A, C)
  val bc = "bc" :: ProdTyp(B, C)
  val abc = "abc" :: ProdTyp(PlusTyp(A, B), C)
  val abc1 = "abc1" :: PlusTyp(ProdTyp(A, C), ProdTyp(B, C))

  val rec1 = ProdTyp(PlusTyp(A, B), C).rec(PlusTyp(ProdTyp(A, C), ProdTyp(B, C)))
  val rec2 = PlusTyp(A, B).rec(PlusTyp(ProdTyp(A, C), ProdTyp(B, C)))
  val rec3 = PlusTyp(ProdTyp(A, C), ProdTyp(B, C)).rec(ProdTyp(PlusTyp(A, B), C))

  val pl1 = PlusTyp(ProdTyp(A, C), ProdTyp(B, C))
  val pl2 = PlusTyp(A, B)

  val pr1 = ProdTyp(A, C)
  val pr2 = ProdTyp(B, C)
  val pr3 = ProdTyp(PlusTyp(A, B), C)

  val f = rec1(ab :-> (c :-> rec2
    (a :-> pl1.incl1(pr1.paircons(a)(c)))
    (b :-> pl1.incl2(pr2.paircons(b)(c)))(ab)))

  val g = rec3(ac :-> pr3.paircons(pl2.incl1(ac.first))(ac.second))(
               bc :-> pr3.paircons(pl2.incl2(bc.first))(ac.second))

  def main(args: Array[String]): Unit = {
    f !: ProdTyp(PlusTyp(A, B), C) ->: PlusTyp(ProdTyp(A, C), ProdTyp(B, C))
    g !: PlusTyp(ProdTyp(A, C), ProdTyp(B, C)) ->: ProdTyp(PlusTyp(A, B), C)
  }
}
