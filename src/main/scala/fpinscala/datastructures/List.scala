package fpinscala.datastructures
import scala.annotation.tailrec

// +A indicates that A is covariant (we can have list of subtypes)
// Sealed means we can only add methods to trait in this file
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]


object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // This is a variadic function meaning it accepts zero or more arguments to conveniently construct data structure
  // The A* type binds as to a Seq with a head, tail and isEmpty method.
  // The _* syntax lets us pass a Seq to the variadic method.
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Pattern matching
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /** Tail removes the first element of list */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("No tail of empty list")
    case Cons(x, t) => t
  }

  /** Set head replaces the head of the list*/
  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => sys.error("Cant set head on empty list")
    case Cons(_, x) => Cons(h, x)
  }

  /** Drops n elements from list starting from front*/
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, x) => drop(x, n-1)
    }
  }

  /** Removes elements from list prefix as long as they match predicate f*/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case Cons(h, t) => Cons(h, dropWhile(t, f))
      case Nil => Nil
  }

  // Note that calling this function we need to assist with the type inference of the
  // anonymous function as dropWhile(l, (x: Int) => x % 2 == 0) if list contains Ints.
  // We can avoid this by currying and call it as dropWhile(l)(x => x % 2 == 0)
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case Cons(h, t) => Cons(h, dropWhile(t, f))
      case Nil => Nil
    }

  /** Removes the last element from the list*/
  def init[A](l: List[A]): List[A] =
  l match {
    case Nil => sys.error("Can't return init of empty MyList")
    case Cons(x, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /* Beauty replaces a list of Cons(1, Cons(2, Nil)) of f(1, f(2, z)) again we curry
  * the function to help with type inference of the anonymous function. Note that
   * foldRight is NOT tail recursive since we stack entire list before doing operations.
   * */
  def foldRight[A, B](l: List[A], z: B)(f:(A,B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  /** Returns the length of a MyList */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  /** Folds a MyList from the left with an accumulator and aggregation function
    * in a tail recursive manner */
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
  l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  /** Returns the sum of a MyList using foldLeft */
  def sum2(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  /** Returns the product of a MyList using foldLeft */
  def product2(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  /** Reverses a list using a foldLeft */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  /** Appends elements of a list to another list using a foldLeft */
  def append[A](l: List[A], r: List[A]): List[A] =
    foldLeft(l, r)((a, acc) => Cons(acc, a))

  /** Concatenates list of lists into one resulting list. Same as flatten in standard library */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  /** Adds one to each element in a List of Integers */
  def addOne(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }

  // Same using a foldLeft
  def addOneAlt(l: List[Int]): List[Int] = foldLeft(l, Nil: List[Int])((t, h) => Cons(h + 1, t))

  /** Converts doubles in List to Strings */
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  /** Modyfing each elemnt in a List according to a function
    * Note that implementing this with a foldLeft does not preserve the ordering
    * This method is not stack safe and could be fixed by implementing foldRight
    * via foldLeft in a tailrecursive manner */
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  /** Filters element of List according to predicate function f */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)
  }

  /** flatMap is the same as applying a map function to each elemnt BUT
    * returning a List as a result. Then all sublists are flattened */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  /* Filter using flatmap */
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) Cons(x, Nil) else Nil)

  /** Constructs a new list from two lists by adding their elements respectively */
  def addPairWise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairWise(t1, t2))
    }

  /** Constructs a new list from two lists pairwise according to a function f */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /** Checks if a list has a given sublist */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sup == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case(_, Nil) => true
    case(Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }
}

/*
* A pure function is a function that does not change data in place or has any side effects
* Functional data structures are persistent meaning that existing references are never changed
* References in functional programming share data meaning that data is never copied
* */