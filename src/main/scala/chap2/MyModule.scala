package chap2

/**
 * 목록 2.1 : 간단한 스칼라 프로그램
 */
object MyModule { // 단일체 객체의 선언, 클래스와 클래스의 유일한 인스턴스를 동시에 선언한다.
  def abs(n: Int): Int =
    if (n < 0) -n
    else n


  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = // 내부함수 또는 지역정의 스칼라에서는 한 함수의 본문에 지역적인 함수를 작성하는 일이 흔하다. 함수형프로그래밍에서 이런 함수는 지역 정수나 문자열과 다를 바 없는 값이다.
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "%d 의 %s은 %d 입니다";
    msg.format(n, name, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("절대값", -42, abs))
    println(formatResult("팩토리얼", 7, factorial))
  }

}
