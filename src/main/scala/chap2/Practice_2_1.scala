package chap2

object Practice_2_1 {

  /**
   *
   * @param n
   * @return
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n <= 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fib(10))
  }

}
