package fpinscala.errorhandling

/* In functional programming we don't use exceptions. Instead we want our functions to return a value when
*  they fail and force the caller to immediately deal failures explicit. We do not want to return some dummy
*  value or null which likely will fail the program silently later and making the functions harder to pass to
*  other higher order functions. Instead we use the Option type. */


/* By keeping all methods inside the trait we */
sealed trait Option[+A]{

  /* Applies function f to the option. If option is empty it returns None */
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /* Applies function f to option and flattens. Can be used on List of Options nicely */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }


  /* Returns the options value if it's nonempty, otherwise return the result of evaluating default.
  *  Note the call by name declaration in input argument. This means that default wont be
  *  evaluated until it's needed in the function. B >: A means that B is supertype of A */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  /* Returns this option if it is nonempty, otherwise return the result os evaluating ob */
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  /** Returns this option if predicate f is true for the nonempty option, otherwise returns None*/
  def filter(f: A => Boolean): Option[A] = {
    if(map(f).getOrElse(false)) this else None
  }

  def filter2(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }


  /* If we have a function A => B we can lift it into a function Option[A] => Option[B].
  *  This is useful when wrapping exception-oriented APIs in Options. We never need to
  *  modfiy any existing functions of one or more arguments to make them Option aware. */

  /** lifts a function into an Option*/
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def lift2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /** Combines a list of Options into one Option containing a list of all the Some values
    * in the original list. If the orininal list contains None even once, the result of the
    * function should be None; otherwise the result should be Some with a list of all the values*/
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



/* An expression is referential transparent, RT, if it can be replaced by it's result
 * without changing the meaning of the program. For instance the expression 2 + 3
 * is an expression that applies the pure function + to 2 and 3. The evaluation of
 * this expression always result in 5 and replacing it with 5 wont change the program*/

/* Example of non-pure nor RT function is StringBuilder append. Consider:
 * val x = new StringBuilder("Hello")
 * val r1 = x.append("World")
 * val r2 = x.append("World")
 * replacing x.append("World") yields "HelloWorld" and "HelloWorldWorld" which is not pure*/

/* Further exceptions are not RT - consider functions below where we in second function only
 * replace y with the expression. Calling first function with 12 throws error. Calling second
 * function with 12 gives 42 since its a catched error. The throw new Exception expression takes
 * on different values depending on the context we put it in. 42 + 5 is always 47 no matter where
 * we put it making it RT. */

/*def failingFn2(i: Int): Int = {
 val y = ((throw new Exception("Fail!")): Int)
 try {
  val x = 42 + 5
  x + y
 }
catch {case e: Exception => 42}
}

def failingFn2(i: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("Fail!")): Int)
  }
  catch {case e: Exception => 42}
}*/