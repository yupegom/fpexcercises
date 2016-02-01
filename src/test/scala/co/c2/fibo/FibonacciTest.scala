package co.c2.fibo

import co.c2.fibo.Fibonacci._
import org.scalatest.FunSuite

class FibonacciTest extends FunSuite {

  test("Fib(0)=0"){
    val result = fib(0)
    assert(result === 0)
  }

  test("Fib(1)=1"){
    val result = fib(1)
    assert(result === 1)
  }

  test("Fib(2)=1"){
    val result = fib(2)
    assert(result === 1)
  }

  test("Fib(3)=2"){
    val result = fib(3)
    assert(result === 2)
  }

  test("Fib(4)=3"){
    val result = fib(4)
    assert(result === 3)
  }

  test("Fib(5)=5"){
    val result = fib(5)
    assert(result === 5)
  }

  test("Fib(6)=8"){
    val result = fib(6)
    assert(result === 8)
  }

}
