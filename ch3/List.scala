sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  // 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // 3.3
  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) => Cons(x, ys)
  }

  // 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0)
      l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(y, Cons(x, xs)) => Cons(y, init(Cons(x, xs)))
  }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product(as: List[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  // 3.7
  // Not really, unless you change things to be lazy, as in Chapter 5

  // 3.8
  // You get the same list. foldRight recreates the List's structure.

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => 1 + b)

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11
  def sum2(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((b, a) => b + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] =    
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))
    
  // 3.13
  def foldLwithR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))
  def foldRwithL[A, B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // 3.14
  def append[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)((a, b) => Cons(a, b))

  // 3.15
  def concatenate[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  // 3.16
  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((element, list) => Cons(element + 1, list))
  
  // 3.17
  def doublesToStrings(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((e, list) => Cons(e.toString, list))

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = 
    foldRight(as, Nil: List[B])((e, list) => Cons(f(e), list))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((e, list) => if (f(e)) Cons(e, list) else list)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((e, list) => append(f(e), list))

  // 3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  // 3.22
  def addPiecewise(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(x, xs) => bs match {
      case Nil => Nil
      case Cons(y, ys) => Cons(x + y, addPiecewise(xs, ys))
    }
  }

  // 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = 
    reverse(foldLeft(as, (Nil: List[C], bs))((acum, a) => acum match {
      case (list, remainderBs) =>
	remainderBs match {
	  case Nil => (list, remainderBs)
	  case Cons(x,xs) => (Cons(f(a, x),list),xs)
	}
    })._1)

  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def isPrefix[A](as: List[A], bs: List[A]) =
	length(as) <= length(bs) && 
      foldRight(zipWith(as, bs)(_ == _), true)(_ && _)
      sup match {
	  case Nil => isPrefix(sub, sup)
	  case Cons(x,xs) => isPrefix(sub, Cons(x,xs)) ||
	    hasSubsequence(xs, sub)
    }
  }
}
