package chapter3

sealed trait Beans[+A]

case object Nil extends Beans[Nothing]

case class Cons[+A](head: A, tail: Beans[A]) extends Beans[A]


object Beans {
  def sum(ints: Beans[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): Beans[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: Beans[A]): Beans[A] = {
    as match {
      case Nil => Nil
      case Cons(_, x) => x
    }
  }

  def head[A](as: Beans[A]): A = {
    as match {
      case Nil => Nil
      case Cons(x, _) => x
    }
  }

  def drop[A](list: Beans[A], n: Int): A = {
  }



  val x = Beans(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
