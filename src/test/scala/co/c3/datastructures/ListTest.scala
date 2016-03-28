
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
    assert(dropWhile(List(1,2,3,4,5), (a:Int) => a == 1) == List(2,3,4,5))
  }

  test("dropWhile in List(2,4,5) x%2 should match List(5)"){
    assert(dropWhile(List(2,4,5), (a:Int) => a % 2 == 0) == List(5))
  }
  
  test("init in List(2,4,5) should match List(2,4)"){
    assert(init(List(2,4,5)) == List(2,4))
  }
  
  test("init in List(2,4,5,6,7,8) should match List(2,4,5,6,7)"){
    assert(init(List(2,4,5,6,7,8)) == List(2,4,5,6,7))
  }
  
  test("length List(2) = 1"){
    assert(length(List(2)) == 1)
  }

  test("length List(2,1,3,4) = 4"){
    assert(length(List(2,1,3,4)) == 4)
  } 
 
  test("foldLeft List(2)  + = 2"){
    assert(foldLeft(List(2),0)(_+_) == 2)
  }
 
  test("foldLeft List(2,3,4,5,6,7)  + = 27"){
    assert(foldLeft(List(2,3,4,5,6,7),0)(_+_) == 27)
  }

  test("foldLeft List(2,3,4,5,6,7)  * = 5040"){
    assert(foldLeft(List(2,3,4,5,6,7),1)(_*_) == 5040)
  }

  test("reverse List(2,3,4,5,6,7) = List(7,6,5,4,3,2)"){
    assert(reverse(List(2,3,4,5,6,7)) == List(7,6,5,4,3,2))
  }

  test("append List(2,3) List(4,5) = List(2,3,4,5)"){
    assert(appendByUsingFoldLeft(List(2,3), List(4,5)) == List(2,3,4,5))
  }

  test("append List(2,3,4,5) List(4,5,6,7,8) = List(2,3,4,5,4,5,6,7,8)"){
    assert(appendByUsingFoldLeft(List(2,3,4,5), List(4,5,6,7,8)) == List(2,3,4,5,4,5,6,7,8))
  }
}
