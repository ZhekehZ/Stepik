package m13_Sigma

import provingground.HoTT._

//noinspection TypeAnnotation
object Task01 {
  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val b = "b" :: B
  val C = "C(_ : A, _ : B)" :: A ->: B ->: Type

  val p = "p" :: Sgma(a !: A, b ~>: C(a)(b))

  val f = p :-> (b :~> mkPair(p.first, p.second(b)))

  def main(args: Array[String]): Unit = {
    f !: Sgma(a !: A, b ~>: C(a)(b)) ->: b ~>: Sgma(a !: A, C(a)(b))
  }
}
