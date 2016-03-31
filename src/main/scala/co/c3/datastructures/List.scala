package co.c3.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil 
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => Nil 
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] = 
   if(n == 0)
     l
   else
     drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil 
      case Cons(x, xs) => 
        if(f(x))
          dropWhile(xs, f)
        else
          l
    }

  def init[A](l: List[A]): List[A] = {
    def aux(listR: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, Nil) => drop(listR, 1)
        case Cons(x, xs) => aux(append(drop(listR, 1), Cons(x, Nil)), xs)
      }
    }
    aux(l, l)
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x,y) => y+1)

  @annotation.tailrec 
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f) 
    }
  }

  def sumFL(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productFL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil:List[A])((x,y) => append(List(y), x))
  }

  def foldLeftByUsingFoldRight[A,B](l: List[A], z: B)(f: (A,B) => B):B = {
    foldRight(reverse(l), z)(f)
  }

  def appendByUsingFoldLeft[A](l: List[A], l1: List[A]): List[A] = {
    foldLeft(reverse(l), l1)((x,y) => Cons(y,x))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil 
    case Cons(h,t) => Cons(f(h), map(t)(f))
  }
}
