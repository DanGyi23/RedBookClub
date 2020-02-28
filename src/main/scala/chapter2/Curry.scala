package chapter2

object Curry {
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    x => y => f(x, y)
}

object Uncurry {
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (x, y) => f(x)(y)
  }
}

object Compose {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
  }
}