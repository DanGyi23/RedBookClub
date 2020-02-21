package chapter2

object IsSorted {
  def isSorted[T](as: Array[T], ordered: (T,T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, state: Boolean): Boolean = {
      if ((n + 1) >= as.length)
        state
      else
        loop(n + 1, state && ordered(as(n), as(n + 1)))
    }
    loop(0, true)
  }
}
