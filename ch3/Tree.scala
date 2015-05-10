sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }
  
  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }
  
  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left,right) => g(fold(left)(f)(g),fold(right)(f)(g))
  }
  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a,b) => a + b + 1)
  def maximum2(t: Tree[Int]): Int =
    fold(t)(x => x)((a,b) => a max b)
  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a,b) => (a max b) + 1)

  // My fold function is analogous to foldRight in that it replaces
  // the type constructors with two other functions.
}
