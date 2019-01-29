package fpinscala.datastructures
import org.scalatest._

class DataStructureSpec extends FlatSpec with Matchers {

  import List._

  "Pattern match" should "the value of x should be 3" in {
    x shouldEqual 3
  }

  "tail" should "return the first element of the list" in {
    tail(List(1,2,3,4)) shouldEqual List(2,3,4)
  }

  it should "throw an error if called on empty MyList" in {
    assertThrows[RuntimeException](tail(Nil))
  }

  "drop while" should "remove all odd numbers from List of ints" in {
    dropWhile(List(1,2,3,4,5), (x: Int) => x % 2 != 0) shouldEqual (List(2,4))
  }

  "init" should "return all elements except the last in a MyList" in {
    init(List(1,2,3,4,5)) shouldEqual List(1,2,3,4)
  }

  "foldright" should "return 10 passing sum function and MyList(1,2,3,4)" in {
    foldRight(List(1,2,3,4), 0)(_ + _) shouldEqual 10
  }

  "length" should "return 5 if the length of MyList is 5" in {
    List.length(List(1,2,3,4,5)) shouldEqual 5
  }

  "product2" should "return 4 when the input is a MyList of (2, 2) using foldLeft" in {
    product2(List(2,2)) shouldEqual 4
  }

  "addOne" should "add 1 to each element in a list of integers" in {
    addOne(List(1,2,3,4)) shouldEqual List(2,3,4,5)
  }

  "doubleToString" should "convert all Doubles in a List to Strings" in {
    doubleToString(List(1.0,2.0,3.0,4.3)) shouldEqual List("1.0", "2.0", "3.0", "4.3")
  }

  "mapAddOne" should "add 1 to each element in a list of integers" in {
    map(List(1,2,3,4))(x => x + 1) shouldEqual List(2,3,4,5)
  }

  "flatMap" should "be able to filter all odd elements in a list" in {
    filter2(List(1,2,3,4,5,6,7,8,9,10))(x => x % 2 == 0) shouldEqual List(2,4,6,8,10)
  }

  "zipWith" should "write (foo, bar) given List(f, ba), List(oo, r)" in {
    zipWith(List("f", "ba"), List("oo", "r"))((a,b) => a + b) shouldEqual List("foo", "bar")
  }

  import Tree._
  val t = Branch(Branch(Leaf(10), Leaf(20)), Leaf(30))

  "maximumTree" should "return 30 if 30 is the biggest Integer in a Tree" in {
    maximum(t) shouldEqual 30
  }

  "mapTree" should "add 10 to each element in a Tree of ints when passing x => x + 10 " in {
    Tree.map(t)(x => x + 10) shouldEqual Branch(Branch(Leaf(20), Leaf(30)), Leaf(40))
  }

  "mapViaFold" should "add 10 to each element in a Tree of ints when passing x => x + 10" in {
    mapViaFold(t)(x => x + 10) shouldEqual Branch(Branch(Leaf(20), Leaf(30)), Leaf(40))
  }
}
