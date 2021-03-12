package m10_Function

import provingground.HoTT._

//noinspection TypeAnnotation
object Task03 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val b = "b" :: B
  val C = "C" :: Type
  val f = "f" :: ProdTyp(A, B) ->: C
  val g = "g" :: A ->: B ->: C
  val p = "p" :: ProdTyp(A, B)

  val curry = f :-> (a :-> (b :-> f(ProdTyp(A, B).paircons(a)(b))))
  val uncurry = g :-> (p :-> g(p.first)(p.second))

  def main(args: Array[String]): Unit = {
    curry !: (ProdTyp(A, B) ->: C) ->: A ->: B ->: C
    uncurry !: (A ->: B ->: C) ->: ProdTyp(A, B) ->: C
  }
}
