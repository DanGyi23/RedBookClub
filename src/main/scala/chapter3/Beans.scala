package chapter3

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

  def append[A](a1: Beans[A], a2: Beans[A]): Beans[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
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
    foldLeft(list, 0)((y, z) => y + z)
  }

  def productFl(list: Beans[Int]): Int = {
    foldLeft(list, 1)((y, z) => y * z)
  }

  def lengthFl[A](list: Beans[A]): Int = {
    foldLeft(list, 0)((z, _) => z + 1)
  }

  def reverse[A](list: Beans[A]): Beans[A] = {
    foldLeft(list, Beans[A]())((b, a) => Cons(a, b))
  }

  def addOne(list: Beans[Int]): Beans[Int] =
    foldRight(list, Nil: Beans[Int])((h,t) => Cons(h + 1,t))

  def doubleToString(list: Beans[Double]): Beans[String] =
    foldRight(list, Nil: Beans[String])((h,t) => Cons(h.toString, t))

  def map[A,B](list: Beans[A])(f: A => B): Beans[B] =
    foldRight(list, Nil: Beans[B])((h,t) => Cons(f(h), t))

  def filter[A](list: Beans[A])(f: A => Boolean): Beans[A] =
    dropWhile(list)(f)

  def concat[A](list: Beans[Beans[A]]): Beans[A] =
    foldRight(list, Nil:Beans[A])(append)

  def flatMap[A,B](list: Beans[A])(f: A => Beans[B]): Beans[B] =
    concat(map(list)(f))

  def FMFilter[A](list: Beans[A])(f: A => Boolean): Beans[A] =
    flatMap(list)(a => if (f(a)) Beans(a) else Nil)

  def zipSummation(list1: Beans[Int], list2: Beans[Int]): Beans[Int] =
    (list1,list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, zipSummation(t1,t2))
    }

  def zipWith[A](list1: Beans[A], list2: Beans[A])(f: (A, A) => A): Beans[A] =
    (list1,list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }


}
