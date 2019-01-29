package fpinscala.gettingstarted

import org.scalatest._

class RecursionSpec extends FlatSpec with Matchers {

  import GettingStarted._

  "fib" should "produce the correct value for n=0" in {
    fib(0) shouldEqual (0)
  }

  it should "produce the correct value for n=1" in {
    fib(1) shouldEqual (1)
  }

  it should "produce the correct value for n=2" in {
    fib(2) shouldEqual (1)
  }

  it should "produce the correct value for n=3" in {
    fib(3) shouldEqual (2)
  }

  it should "produce the correct value for n=4" in {
    fib(4) shouldEqual (3)
  }

  it should "produce the correct value for n=5" in {
    fib(5) shouldEqual (5)
  }

  "isSorted" should "return true for lessThan comparsion of sorted list of ints" in {
    val sortedList = List(1,2,3,4,5,6,7,8,9,10)
    isSorted(sortedList, (a: Int, b: Int) => a <= b) shouldEqual (true)
  }

  it should "return false for lessThan comparsion of non-sorted list of ints" in {
    val unsortedList = List(1,2,3,4,5,6,7,6,5,4)
    isSorted(unsortedList, (a: Int, b: Int) => a <= b) shouldEqual(false)
  }

  "isSortedTailRect" should "return true for lessThan comparsion of sorted list of ints" in {
    val sortedList = List(1,2,3,4,5,6,7,8,9,10)
    isSortedTailRec(sortedList, (a: Int, b: Int) => a <= b) shouldEqual (true)
  }

  it should "return false for lessThan comparsion of non-sorted list of ints" in {
    val unsortedList = List(1,2,3,4,5,6,7,6,5,4)
    isSortedTailRec(unsortedList, (a: Int, b: Int) => a <= b) shouldEqual(false)
  }

  "compose" should "take two functions and apply them as f1(f2(value))" in {
    def plusOne(n: Int) = n + 1
    def minusOne(n: Int) = n -1
    def composed = compose(plusOne, minusOne)
    composed(10) shouldEqual (10)
  }

}
