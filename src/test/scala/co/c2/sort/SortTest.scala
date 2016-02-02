package co.c2.sort

import org.scalatest.FunSuite

class SortTest extends FunSuite {

  def ordered(itemA: String, itemB: String): Boolean = itemA < itemB

  def orderedInt(itemA: Int, itemB: Int): Boolean = itemA < itemB


  test("array = Array(\"A\", \"B\", \"C\") is sorted"){
    val array = Array("A", "B", "C")
    val result = Sort.isSorted(array, (itemA: String, itemB: String) => itemA < itemB)
    assert(result)
  }

  test("array = Array(\"A\", \"C\", \"B\") is not sorted"){
    val array = Array("A", "C", "B")
    val result = Sort.isSorted(array, ordered)
    assert(!result)
  }

  test("array = Array(\"A\", \"B\", \"C\", \"D\", \"F\") is sorted"){
    val array = Array("A", "B", "C", "D", "F")
    val result = Sort.isSorted(array, ordered)
    assert(result)
  }

  test("array = Array(\"A\", \"B\", \"C\", \"D\", \"C\") is not sorted"){
    val array = Array("A", "B", "C", "D", "C")
    val result = Sort.isSorted(array, ordered)
    assert(!result)
  }

  test("array = Array(1, 3, 4) is sorted"){
    val array = Array(1, 3, 4)
    val result = Sort.isSorted(array, orderedInt)
    assert(result)
  }

  test("array = Array(1, 3, 4, 2) is not sorted"){
    val array = Array(1, 3, 4, 2)
    val result = Sort.isSorted(array, orderedInt)
    assert(!result)
  }


}
