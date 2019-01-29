package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /** Returns the total number of nodes and leafs in a tree*/
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  /** Returns the maximum number in a Tree of Integers */
  def maximum(t: Tree[Int]): Int =
    t match {
      case Branch(l,r) => maximum(l) max maximum(r)
      case Leaf(n) => n
  }

  /** Returns the longest distance from the root to a leaf */
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l) max depth(r)
    }

  /** Returnes a new tree where function f has been applied for each element*/
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  /* As with rightFold on lists we can generalise all above implementations to
  *  a fold function both do aggregations or construct new trees by passing a
  *  constructor to the accumulator. Note that we need two handler functions for
  *  the tree since it has two different constructors - Branch and Leaf */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(n => 1)((l, r) => 1 + l + r)

  def maxViaFold[A](t: Tree[Int]): Int =
    fold(t)(n => n)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(n => 0)((l, r) => 1 + l max r)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}
