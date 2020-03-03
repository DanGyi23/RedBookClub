package chapter3

import chapter3.Beans.x

sealed trait Beans[+A]

case object Nil extends Beans[Nothing]

case class Cons[+A](head: A, tail: Beans[A]) extends Beans[A]


object Beans {
  def sum(ints: Beans[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: Beans[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): Beans[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: Beans[A]): Option[Beans[A]] = {
    as match {
      case Cons(_, x) => Some(x)
      case _ => None
    }
  }

  def head[A](as: Beans[A]): Option[A] = {
    as match {
      case Cons(x, _) => Some(x)
      case _ => None
    }
  }

  def setHead[A](as: Beans[A], newHead: A): Beans[A] = {
    Cons(newHead, tail(as).getOrElse(Nil))
  }

  def drop[A](list: Beans[A], n: Int): Beans[A] = {
    def loop(z: Int, modTail: Beans[A]): Beans[A] = {
      if (z == 0) modTail
      else loop(z - 1, tail(modTail).getOrElse(Nil))
    }

    loop(n, list)
  }

  def dropWhile[A](list: Beans[A])(f: A => Boolean): Beans[A] = {
    list match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => list
    }
  }

  def foldRight[A, B](list: Beans[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def length[A](list: Beans[A]): Int = {
    foldRight(list, 0)((_, xs) => xs + 1)
  }

  def foldLeft[A, B](list: Beans[A], z: B)(f: (B, A) => B): B = {
    list match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumFl(list: Beans[Int]): Int = {
    foldLeft(list, 0)((y,z) => y + z)
  }

  def productFl(list: Beans[Int]): Int = {
    foldLeft(list, 1)((y,z) => y * z)
  }

  def lengthFl[A](list: Beans[A]): Int = {
    foldLeft(list, 0)((z, _) => z + 1)
  }

  //  def dropLast[A](list: Beans[A]): Beans[A] = {
  //    def loop(list: Beans[A]) = {
  //
  //    }
  //  }


  val x = Beans(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
