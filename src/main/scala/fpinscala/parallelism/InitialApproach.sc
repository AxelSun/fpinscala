
object Examples {

  /* The non-parallel sequential approach to sum a list of integers*/
  def sumSeq(l: List[Int]): Int = l.foldLeft(0)(_ + _)

  /* The divide and conquer appraoch to sum a list. In theory the two halves
  *  can be computed in parallel */
  def sumDivConq(l: IndexedSeq[Int]): Int = {
    if(l.size <= 1) l.headOption.getOrElse(0)
    else {
      val (left, right) = l.splitAt(l.length / 2)
      sumDivConq(left) + sumDivConq(right)
    }
  }

}

import Examples._

val list_ = List(1,2,3,4,5,6,7,8)

sumSeq(list_)

sumDivConq(list_.toIndexedSeq)


// Represent a container for a parallel computation
case class Par[A](a: A)

/* Transforms an unevaluated a and returns a computation that might take place
*  in some thread */
def unit[A](a: => A): Par[A]

/* To extract the result from a parallel computation */
def get[A](a: Par[A]): A = ???

/* map2 is a higher-order function for combining the result of two parallel computations */
def map2[A, B, C](a: Par[A], b: Par[A])(f: (A, B) => C): Par[C] = ???


def sumPar(ints: IndexedSeq[Int]): Par[Int] = {
  if(ints.size <= 1)
    unit(ints.headOption.getOrElse(0))
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    map2(sumPar(l), sumPar(r))(_ + _)
  }
}
