package co.c2.curry

import co.c2.curry.Currying._
import org.scalatest.FunSuite


class CurryingTest extends FunSuite {

  test("curry(2)(3) (a, b) => a + b)=5"){
    val f = curry((a: Int, b: Int) => a + b)(2)
    assert(f(3) == 5)
  }

  test("curry(3)(3) (a, b) => a - b) =0"){
    val f = curry((a: Int, b: Int) => a - b)(3)
    assert(f(3) == 0)
  }

}
