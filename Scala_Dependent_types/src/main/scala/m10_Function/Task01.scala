package m10_Function

import provingground.HoTT._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val C = "C" :: Type
  val f = "f" :: A ->: B
  val g = "g" :: B ->: C

  val comp = f :-> (g :-> (a :-> g(f(a))))

  def main(args: Array[String]): Unit = {
    comp !: (A ->: B) ->: (B ->: C) ->: A ->: C
  }
}
