package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }


  def randomPlease(randyRando: RNG): (Int, RNG) = {
    val (i, r) = randyRando.nextInt
    i match {
      case n if n < 0 => (-n + 1, r)
      // if MinValue, make the negative value positive and add one, since MinValue is 1 smaller than negative MaxValue
      case _ => (i, r)
      // if not minvalue, just return what the rng generates
    }
  }

  def randomDouble(rng: RNG): (Double, RNG) = {
    val (i, r) = randomPlease(rng)
    i match {
      case n if n == 1 => n / Int.MaxValue
      // handle i if it's one, just divide it by a static value. Maxvalue could be anything here, as long as it's constant.
      // This pattern match just removes possibility of returning 1.
    }
    (i.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = randomPlease(rng)
    // get random int
    val (d, r2) = randomDouble(r)
    // use the resulting RNG to seed the random double
    ((i, d), r2)
    // return the resulting RNG of the double function, along with the i, d generated by both function calls.
  }

  def doubleDouble(rng: RNG): ((Double, Double), RNG) = {
    val (d, r) = randomDouble(rng)
    val (d2, r2) = randomDouble(r)
    ((d, d2), r2)
    // same logic as intDouble function
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = randomDouble(rng)
    val (d2, r2) = randomDouble(r)
    val (d3, r3) = randomDouble(r2)
    ((d, d2, d3), r3)
    //samesies
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(innerCount: Int, list: List[Int], innerRng: RNG): (List[Int], RNG) = {
      if (innerCount == 0) (list, innerRng)
      // return the list so far when count reaches zero (don't add any more ints)
      else {
        val (i, r2) = innerRng.nextInt
        // get the next (integer, RNG) pair using the RNG provided after execution of each loop (innerRNG)
        loop(innerCount - 1, i :: list, r2)
        // then loop, reducing the count by one each time, Cons(Int, list), and passing in the NEW RNG to the recursive call.
      }
    }

    loop(count, List.empty, rng)
    // initiate with an empty list
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt


  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleWithMap: Rand[Double] = {
    map(randomPlease)(_.toDouble)
    // randomPlease follows type pattern RNG => (A, RNG), so satisfies data type Rand[Double]
    // .map satisfied since type changes from Int to Double
    // .map also preserves RNG propagation, so preserves functionality
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // similar ideas to previous methods, you're just propagating the rng value between consecutive RNG executions.

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDoubleUsingBoth: Rand[(Int, Double)] =
    both(int, randomDouble)

  def randDoubleIntUsingBoth: Rand[(Double, Int)] =
    both(randomDouble, int)

  def sequence[A](transitions: List[Rand[A]]): Rand[List[A]] = {
    transitions.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    // looked up!
  }

  def intsUsingSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    rng => {
      val (i, rng2) = randomPlease(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng2)
    }

  def mapFM[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))
    // (x => unit(f(x))) just transforms A to B

  def map2FM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
    // chains map operations to isolate A, B, so that function f can take as params


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r1) = f(rng)
      // using the result of your first Rand[A] call, you're transforming the A result to Rand[B]
      g(a)(r1)
    }
  }


}




