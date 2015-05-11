package ch4

sealed trait Either[+E,+A] {
  // 4.6
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for {
      x <- this
      y <- b
    } yield f(x,y)
  
}
case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // 4.7
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    as.foldRight[Either[E,List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(identity)
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))

  // 4.8
  // Here is one possibility. We store on Option[String] per possible error
  def otherMkPerson(name: String, age: Int): Either[
    List[Option[String]], Person] = {
    val a = mkAge(age)
    mkName(name) match {
      case Left(e1) => a match {
	case Left(e2) => Left(List(Some(e1), Some(e2)))
	case Right(x) => Left(List(Some(e1), None))
      }
      case Right(n) => a match {
	case Left(e2) => Left(List(None, Some(e2)))
	case Right(x) => Right(new Person(n, x))
      }
    }
  }

  def main(args: Array[String]) = {
    println(otherMkPerson(args(0), args(1).toInt))
  }
}
