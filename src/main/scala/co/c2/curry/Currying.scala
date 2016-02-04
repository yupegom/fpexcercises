package co.c2.curry

object Currying {

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

}
