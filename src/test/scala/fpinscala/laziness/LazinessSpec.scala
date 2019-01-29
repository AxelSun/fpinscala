package fpinscala.laziness

import org.scalatest._

class LazinessSpec extends FlatSpec with Matchers {

  import Stream._

  "take" should "return a list of 2 integers from a Stream(1,2,3) when called upon take(2)" in {
    Stream(1,2,3).take(2).toList shouldEqual List(1,2)
  }

  "drop" should "remove the first element and return a list of 2 elements" in {
    Stream(1,2,3).drop(2).toList shouldEqual List(3)
  }

  "takeWhile" should "return first 3 elements from a Stream" in {
    Stream(2, 4, 6, 7, 9, 10, 0, 13, 2).takeWhile((x: Int) => x % 2 == 0).toList
      .shouldEqual(List(2,4,6))
  }


}
