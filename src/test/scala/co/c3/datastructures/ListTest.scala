
package co.c3.datastructures

import co.c3.datastructures.List._
import org.scalatest.FunSuite


class ListTest extends FunSuite {

  test("List(1,2,3,4,5) should match "){
    assert(x == 3)
  }

  test("tail List(1,2,3,4,5) should match List(2,3,4,5"){
    assert(tail(List(1,2,3,4,5)) == List(2,3,4,5))
  }

  test("setHead 5 in List(1,2,3,4,5) should match List(5,2,3,4,5"){
    assert(setHead(List(1,2,3,4,5), 5) == List(5,2,3,4,5))
  }
  
  test("drop 5 in List(1,2,3,4,5) should match Nil"){
    assert(drop(List(1,2,3,4,5), 5) == Nil)
  
  }

  test("drop 4 in List(1,2,3,4,5) should match List(5)"){
    assert(drop(List(1,2,3,4,5), 4) == List(5))
  }

  test("drop  in List(1,2,3,4,5) should match List()"){
    assert(drop(List(1,2,3,4,5), 6) == List())
  }

  test("dropWhile in List(1,2,3,4,5) x==1 should match List(2,3,4,5)"){
    assert(dropWhile(List(1,2,3,4,5), a:Int => a == 1) == List(2,3,4,5))
  }

}
