package co.c2.sort

import scala.reflect.ClassTag

object Sort {

  def isSorted[A: ClassTag](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A]): Boolean = as match {
      case Array(a,b @_*) =>
        if(b.isEmpty) true
        else if(ordered(a, b.head) ) {
          loop(b.toArray[A])
        }
        else {
          false
        }
    }
    loop(as)
  }

}
