package chap2

/**
 * 목록 2.1 : 간단한 스칼라 프로그램
 */
object MyModule { // 단일체 객체의 선언, 클래스와 클래스의 유일한 인스턴스를 동시에 선언한다.

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int):Int = // 내부함수 또는 지역정의 스칼라에서는 한 함수의 본문에 지역적인 함수를 작성하는 일이 흔하다. 함수형프로그래밍에서 이런 함수는 지역 정수나 문자열과 다를 바 없는 값이다.
      if (n <= 0 ) acc
      else go(n-1, n*acc)
    go(n,1)
  }

  def abs (n: Int): Int =
    if (n < 0) -n
    else n


  private def formatAbs(x: Int) = { // 전용(private) 메서드는 오직 MyModule의 다른 멤버들만 호출 할수 있다.
    val msg = "%d 의 절대값은 %d 입니다."
    msg.format(x ,abs(x))
  }

  def main(args: Array[String]): Unit = { // Unit은 자바에서 Void와 동일하다.
    println(formatAbs(-42))
  }


}
