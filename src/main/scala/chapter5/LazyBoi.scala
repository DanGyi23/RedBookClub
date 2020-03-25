package chapter5

import chapter3.Beans


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def toListLazy: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buffer += h()
        loop(t())
      case _ => buffer.toList
    }

    loop(this ())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    // if n is greater than 1, you want to recursively loop through the tail, removing n elements until n is equal to 1
    // [1,2,3] becomes Cons(1, [2,3]), becomes [1] :: Cons(2, 3) ---- if n is 2, you end up with { 1 :: Cons(2, empty) }
    case Cons(h, _) if n == 1 => cons(h(), empty)
    // once n reaches 1, you want to return only the head (which can be composed of multiple Cons branches)
    case _ => empty
    // if n < 1, you won't return any Stream elements
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    // if  n is non-zero, you want to return the tail of the list n times =>
    // [1,2,3] tail becomes [2,3] tail becomes [3]
    case _ => this
    // Or else just return the same stream, since you're not dropping a positive integer of elements
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    // if the predicate is true for Head, keep the head and recursively check the head of each tail.
    // Halt recursion and set the tail to empty once the predicate is false
    // For [1,2,3] takeWhile( A < 3 ), we get 1 :: [2,3], then 1 :: 2 :: empty, which is equivalent to [1,2]
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int, s: Stream[A]): Boolean = this match {
      case Cons(h, t) =>
        if (p(h()) && (n == 0)) {
          true
          // if we reach n == 0 without having returned a false, the expression must be true
        } else if (p(h()) && n != 0) {
          loop(n - 1, t())
          // if n is still not 0, we must loop until it is (whilst p(h()) remains true)
        }
        else false
        // if p(h()) ever evaluates to false, we return false immediately
      case _ => false
        // if this is not of the type Cons(h,t), we return false by default
    }
    loop(this.toListLazy.length, this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def takeWhileFR(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h,t) =>
      // into an empty Stream, conditionally fold if the head matches a predicate, otherwise stop and return Cons(StreamSoFar, empty)
      if (p(h)) cons(h,t)
      else empty)
  }

  def mapFR[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h,t) =>
      cons(f(h), t()))
    // just apply the parameterised function to every head element as you fold
    // [1,2,3].mapFR(_ * 2) -----> 2 + (foldRight(Cons(2, Cons(3, Nil))) ------> 2 + (4 + (foldRight(Cons(3,Nil))) ------> (2 + (4 + (6 + (0))))
  }

  def filterFR(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) =>
    if (f(h)) cons(h, t)
      // keeps head if f(h) is true
    else t
      // discards the head if (f(h)) evaluates to false
    )
  }

  def appendFR[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMapFR[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h,t) => f(h).appendFR(t))

  }
}
