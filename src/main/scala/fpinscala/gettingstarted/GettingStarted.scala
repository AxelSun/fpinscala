package fpinscala.gettingstarted
import scala.annotation.tailrec


object GettingStarted {

  // Exercise 2.1 - Fibonacci nth recursively
  def fib(n: Int): Int = {
    def loop(idx: Int, prev: Int, curr: Int): Int = {
      if(idx == 0) prev
      else loop(idx-1, curr, prev + curr)
    }
    loop(n, 0, 1)
  }

  // Exercise 2.2 - Implement isSorted which checks whether a List[T]
  // is sorted according to a given comparsion method
  def isSorted[T](as: List[T], f: (T,T) => Boolean): Boolean = as match {
    case x :: Nil => true
    case x :: xs => {
      if(f(x, xs.head)) isSorted(xs, f)
      else false
    }
    case Nil => true
  }

  // tail recursive version of method above
  def isSortedTailRec[T](as: List[T], f: (T,T) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else if (f(as(n), as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  // This return type associates to the right which means it is the same thing to
  // write A => B => C
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /** Composes two functions as f(g(x)) */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
