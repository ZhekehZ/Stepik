package m07_TypeClasses

object Task02 {
  trait Foldable[T[_]] {
    def foldr[A, B](ta: T[A])(f: A => B => B)(seed: B): B
  }

  sealed trait BTree[+A]
  case class Empty[A]() extends BTree[A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], a: A, right: BTree[A]) extends BTree[A]

  implicit object foldableBTree extends Foldable[BTree] {
    override def foldr[A, B](ta: BTree[A])(f: A => B => B)(seed: B): B = ta match {
      case Leaf(l) => f(l)(seed)
      case Fork(l, a, r) => foldr(r)(f)(f(a)(foldr(l)(f)(seed)))
      case Empty() => seed
    }
  }

  implicit object foldableFork extends Foldable[Fork] {
    override def foldr[A, B](ta: Fork[A])(f: A => B => B)(seed: B): B = implicitly[BTree[A]](ta).foldr(f)(seed)
  }

  implicit object foldableLeaf extends Foldable[Leaf] {
    override def foldr[A, B](ta: Leaf[A])(f: A => B => B)(seed: B): B = implicitly[BTree[A]](ta).foldr(f)(seed)
  }

  implicit object foldableEmpty extends Foldable[Empty] {
    override def foldr[A, B](ta: Empty[A])(f: A => B => B)(seed: B): B = implicitly[BTree[A]](ta).foldr(f)(seed)
  }

  implicit class WithFoldr[F[_], A](fa: F[A]) {
    def foldr[B](f: A => B => B)(seed: B)(implicit foldable: Foldable[F]): B = foldable.foldr(fa)(f)(seed)
  }

  def main(args: Array[String]): Unit = {
    val tree = Fork(Leaf(1), 2, Leaf(3))

    tree.foldr((x: Int) => (y: Int) => x + y)(0)

    assert(
      implicitly[Foldable[BTree]].foldr(tree)((x: Int) => (y: Int) => x + y)(0) == 6 )
  }

}
