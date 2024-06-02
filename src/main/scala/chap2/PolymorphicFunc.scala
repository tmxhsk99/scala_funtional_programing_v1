package chap2

/**
 * 다형적 함수
 */
object PolymorphicFunc {

  /**
   * 배열에서 문자열을 찾는 단형적 함수
   *
   * @param ss
   * @param key
   * @return
   */
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1 // 만일 n이 배열의 끝을 지나쳤다면, 배열의 키가 존재하지 않음을 뜻하는 값인 -1을 돌려준다.
      else if (ss(n) == key) n // ss(n)은 배열의 n번째 요소의 값, n번째요소가 키와 상등이면, n을 돌려준다, 해당 요소가 그 색인에 존재함을 뜻한다.
      else loop(n + 1)

    loop(0) // 배열의 첫요소에서 루프를 시작한다.
  }

  /**
   * 목록 2.4 배열에서 한 요소를 찾는 다형적 함수
   * @param as
   * @param p
   * @tparam A
   */
  def findFirst[A](as: Array[A], p: A => Boolean): Unit = { // String을 코드에 박아넣는 대신, 형식 A를 하나의 인수로 받는다, 그리고 주어진 키와의 상등 판정을 하드코딩하는 대신 배열의 각 요소를 검사하는 함수를 받는다.
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n // 함수 p가 현재 요소와 부합한다면, 원하는 요소를 찾은것이므로 배열안에서 해당색인을 돌려준다.
      else loop(n + 1)
    }
    loop(0)
  }

}
