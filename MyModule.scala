object MyModule {
  def abs(n: Int): Int = 
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n,1)
  }

  def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n-1) + fib(n-2)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  /**
   * Monomorphic function to find a String in an array.
   * @param ss Array to process.
   * @param key What to search within the array.
   * @return Index in the array for the given String, -1 if not found.
   */
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)

    loop(0)
  }

  /**
   * Polymorphic function to find a String in an array.
   * <strong>Usage:</strong> <em>findFirst(array, function)</em><br />
   * <strong>e.g.:</strong> <code>findFirst(Array(2,8,10), (x: Int) => x == 8)</code>
   * @param as Typed array.
   * @param p Predicated function that evaluates a given key.
   * @return Index in the array for the given string, -1 if not found.
   */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  /**
   * Polymorphic function to define if an array is sorted.
   * @param as Typed array.
   * @param ordered Predicated function who defines if a tuple is sorted.
   * @return True if the array is ordered, false otherwise.
   */
  def sorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(l: Int, h: Int): Boolean =
      if ((h <= 0) || (l <= 0)) true
      else if (!ordered(as(l), as(h))) false
      else loop(l-1, h-1)

    loop(as.length-2, as.length-1)
  }

  /**
   * Partial function which apply to some of the arguments but not all.
   * @param a Initial argument of type A.
   * @param f HOF to use as a bait.
   * @return A B type result.
   */
  def partial[A,B,C](a: A, f: (A, B) => C): B => C =
    b => f(a,b) // or -> (b: B) => f(a,b)

  /**
   * Currying transforms any method with high aries into multiple one-aried ç
   * functions, hence favoring abstraction.
   * <strong>Usage:</strong> <em>curry(function)(param1)(param2)</em>
   * <strong>e.g.:</strong>  <code>curry((x: Int, y: Int) => x+y)(2)(3)</code>
   * @param f HOF to be applied once all the parameters are provided.
   * @return successive functions to apply over the paramters (two in this case)
   */
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  /**
   * Reverts the <code>curry</code> function.
   * <strong>Usage:</strong> <em>uncurry(function)(x,y)</em>
   * <strong>e.g.:</strong>
   * <code>
   *  val f = curry((x: Int, y: Int) => x*y)
   *  val uc = uncurry(f)
   *  uc(5,2)
   * </code>
   * @param f HOF whic curries two parameters.
   * @return Single function expanding the given two parameters.
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a, b)

  /**
   * Given two HOF, evaluates them into a single function,
   * <strong>Usage:</strong> <em>compose(f,g)</em>
   * <strong>e.g.:</strong>
   * <code>
   *  val f = (x: Int) => x+2
   *  val g = (y: Int) => y*3
   *  val c = compose(f,g)
   *  c(2)
   * </code>
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
