/**
  * Created by aliszka on 02/04/17.
  */
object MyModule {
  def abs(x: Int): Int =
    if (x < 0) -x
    else x

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 1) acc
      else loop(n - 1, n * acc)

    loop(n, 1)
  }

  def fibonacci(n: Int): Int = {
//    @annotation.tailrec
//    def loop(a: Int, b: Int, i: Int, n: Int): Int =
//      if (n == i) b
//      else loop(b, a + b, i + 1, n)
//
//    if (n <= 1) 0
//    else loop(0, 1, 2, n)

//    @annotation.tailrec
//    def loop(a: Int, b: Int, n: Int): Int =
//      if (n <= 1) a
//      else loop(b, a + b, n - 1)
//
//    loop(0, 1, n)

    @annotation.tailrec
    def loop(a: Int, b: Int, n: Int): Int =
      if (n <= 2) b
      else loop(b, a + b, n - 1)

    if (n <= 1) 0
    else loop(0, 1, n)
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
    println(formatResult("factorial", 42, factorial))

  }
}
