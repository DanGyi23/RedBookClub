package chapter6

case class State[S, +A](run: S => (A, S)) {
  type Rand[A] = State[RNG, A]
  import State.unit


  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B, C](initialState: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => initialState.map(b => f(a, b)))
  // takes an initial state, then applies the function f to the result of the two operations A, B, whilst retaining knowledge of the State.
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
    // rather than having to propagate the RNG state through the function,
    // the State case class stores the [S] automatically, so this is all done in the function signature!
    // looked some of this up...
  })


}


object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}
