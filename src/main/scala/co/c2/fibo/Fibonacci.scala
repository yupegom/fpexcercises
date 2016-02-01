package co.c2.fibo


object Fibonacci {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int, acc2: Int): Int = n match {
      case 0 => acc
      case 1 => acc2
      case _ => loop(n-1, acc2, acc + acc2)
    }
    loop(n, 0, 1)
  }

}
