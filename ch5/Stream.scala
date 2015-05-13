package ch5

import Stream._
trait Stream[+A] {
  // 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => loop(t(), h()::l)
    }
    loop(this, List()).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    def loop(s: Stream[A], n: Int): Stream[A] = n match {
      case 0 => Empty
      case x => s match {
	case Empty => Empty
	case Cons(h, t) => Cons(h, () => loop(t(), n - 1))
      }
    }
    loop(this, n)
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], n: Int): Stream[A] = n match {
      case 0 => s
      case x => s match {
	case Empty => Empty
	case Cons(h, t) => loop(t(), x-1)
      }
    }
    loop(this, n)
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  // 5.5
  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  // 5.6
  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, t) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((h,t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](xs: Stream[B]): Stream[B] =
    foldRight(xs)((h,t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){case Empty => None
		case Cons(h,t) => Some((f(h()),t()))}

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)){case (0, _) => None
		      case (_, Empty) => None
		      case (m, Cons(h,t)) => Some((h(),(m-1,t())))}

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){case Empty => None
		 case Cons(h,t) => if (p(h())) Some((h(), t())) else None}

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this, s){
      case (Empty,_) => None
      case (_,Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(),h2()), (t1(),t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, s2){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())),
						(t1(),t2()))
      case (Empty, Cons(h2,t2)) => Some((None, Some(h2())), (Empty,t2()))
      case (Cons(h1,t1), Empty) => Some((Some(h1()),None), (t1(),Empty))
      case _ => None
    }

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).map {
      case (_,None) => true
      case (None,_) => false
      case (Some(a),Some(b)) => a == b
    }.forAll(x => x)

  // 5.15
  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case Cons(h,t) => Some((Cons(h,t),t()))
    }.append(Stream(empty[A]))

  // 5.16
  def scanRight[B](z: B)(f: (A,B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h,t) => {
      lazy val tail = t().scanRight(z)(f)
      tail match {
	case Cons(x,_) => cons(f(h(),x()), tail)
      }
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A,t: () =>Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def loop(x: Int, y: Int): Stream[Int] =
      cons(x, loop(y, x+y))
    loop(0,1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // 5.12
  def ones: Stream[Int] =
    unfold(Nil)(_ => Some((1, Nil)))

  def constant2[A](a: A): Stream[A] =
    unfold(Nil)(_ => Some((a, Nil)))

  def from2(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def fibs2: Stream[Int] =
    unfold((0, 1)){case (x,y) => Some((x,(y,x+y)))}
}
