package fpinscala.state
import fpinscala.state.RNG.Rand

/* TA way to handle states purely functional and not break referential transparency or cause side
 * effects is to return a new state along with the value we're generating. This make state update explicit*/

/* Use trait when introducing a data structure */
trait RNG {
  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


object RNG {
  /* Following six operations in this API implementation is of type (RNG) => (A, RNG) for some type A.
   * Functions of this type is called state. This state actions can be combined using combinators */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    if(i < 0) (-(i + 1), rng1) else (i, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng1)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d.toDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(xs: List[Int], count: Int, rng: RNG):(List[Int], RNG) = {
      if(count == 0) (xs, rng)
      else {
        val (i, rngNext) = rng.nextInt
        loop(i :: xs, count - 1, rngNext)
      }
    }
    loop(Nil, count, rng)
  }

  /* This is a combinator and could be thought of as - a randomly generated A. Or more precise - this is a program
   * that depends on some RNG, uses it to generate an A, also transitions the RNG to a new state that can be used
   * by another action. This creates a domain specific language. */
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def int2: Rand[Int] = _.nextInt

  def double2: Rand[Double] =
    map(nonNegativeEven)(i => i / (Int.MaxValue.toDouble + 1))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((a,b) => (a, b))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int2))

  val randIntDouble: Rand[(Double, Int)] = both(double2, int2)

  val randDoubleInt: Rand[(Int, Double)] = both(int2, double2)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((i, acc) => map2(i, acc)(_ :: _))

  def nonNegtiveLessThan(n: Int): Rand[Int] = {rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if(i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegtiveLessThan(n)(rng2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val(i, rng2) = f(rng)
    g(i)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapfm[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => mapfm(rb)(b => f(a,b)))
}

import State._

/** We wrap the more general type State[S, +A] directly in the case class. State is short for computation that carries
  * some state along, or state action, state transition, or even statement. The function taking a state and returns a value
  * a and new state can be wrapped in a case class nicely */
case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

}

object State {

  /** Takes some general State and returns a value along with a new state*/
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  /** Tail recursive and preserves the order */
  def sequence[S,A](xs: List[State[S, A]]): State[S, List[A]] =
    xs.reverse.foldLeft(unit[S, List[A]](List()))((acc, i) => i.map2(acc)( _ :: _ ))

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def update = (i: Input) => (m: Machine) =>
    (i, m) match {
      // Inserting a coin into unlocked machine does nothing
      case (Coin, Machine(false, _, _)) => m
      // A machine that is out of candy ignores input
      case (_, Machine(false, 0, _)) => m
      //inserting coin to locked machine will cause it to unlock if there is candy left
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      // Turning the knob on unlocked machine will cause it to dispense candy and become locked
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      // Turning the knob on locked machine does nothing
      case (Turn, Machine(true, _, _)) => m
    }


  def update2(i: Input, m: Machine): Machine =
    (i, m) match {
      case (Coin, Machine(false, _, _)) => m
      case (_, Machine(false, 0, _)) => m
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
      case (Turn, Machine(true, _, _)) => m
    }


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

}
