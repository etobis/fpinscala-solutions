package ch4

import scala.{Option => _, Some => _, None => _, _}

trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map (Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap (a => if (f(a)) Some(a) else None)
    
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap (m => mean(xs.map (x => math.pow(x - m, 2))))
  }

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x,y)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x,y)(_ :: _))

  // 4.5
  // Inefficient implementation
  def traverse2[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map2(f(x),y)(_ :: _))

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

}
