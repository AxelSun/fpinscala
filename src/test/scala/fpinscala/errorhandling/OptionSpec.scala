package fpinscala.errorhandling

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  "filter" should "return None if x < 5 and Some(10) if x > 5 for Some(10)" in {
    Some(10).filter(_ < 5) shouldEqual None
    Some(10).filter(_ > 5) shouldEqual Some(10)
  }

}
