package m07_TypeClasses

//import scala.language.higherKinds

object Task01 {
  trait Monad[M[_]] {
    def bind[A, B](ma: M[A], k: A => M[B]): M[B]
    def pure[A](a: A): M[A]
  }

  implicit object listMonad extends Monad[List] {
    override def bind[A, B](ma: List[A], k: A => List[B]): List[B] = ma.flatMap(k)

    override def pure[A](a: A): List[A] = List(a)
  }

  implicit class WithBind[A, M[_]: Monad](m : M[A]) {
    def >>=[B] (k: A => M[B]): M[B] = implicitly[Monad[M]].bind(m, k)
  }

  implicit class WithPure[A](x : A) {
    def pure[M[_]: Monad]: M[A] = implicitly[Monad[M]].pure(x)
  }

  val as: List[String] = List("a", "b", "c")
  val k: String => List[String] = x => List(x + "0" , x + "1")

  def main(args: Array[String]): Unit = {
    assert(
      implicitly[Monad[List]].bind(as, k) ==
        List("a0", "a1", "b0", "b1", "c0", "c1")
    )
    assert((as >>= k) == List("a0", "a1", "b0", "b1", "c0", "c1"))

    assert(implicitly[Monad[List]].pure("a") == List("a"))
    assert("a".pure == List("a"))
    assert(10.pure[List] == List(10))
  }

}
