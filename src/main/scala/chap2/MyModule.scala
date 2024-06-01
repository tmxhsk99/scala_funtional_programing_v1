package chap2

/**
 * 목록 2.1 : 간단한 스칼라 프로그램
 */
object MyModule { // 단일체 객체의 선언, 클래스와 클래스의 유일한 인스턴스를 동시에 선언한다.
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
