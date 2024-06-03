package chap2

/**
 * 두 함수를 합성하는 고차 함수를 구현 하라
 */
object Practice_2_5 {
  def compose[A,B,C] (f: B => C, g:A => B): A => C = (a:A) => f(g(a))

  def main(args: Array[String]): Unit = {
    // 예제 함수
    val double: Int => Int = x => x * 2
    val increment: Int => Int = x => x + 1

    // 두 함수를 합성
    val composedFunction = compose(double, increment)

    // 테스트 데이터
    val testData = List(1, 2, 3, 4, 5)

    // 합성된 함수 적용
    val results = testData.map(composedFunction)

    // 결과 출력
    println("Test Data: " + testData.mkString(", "))
    println("함수 합성 결과: " + results.mkString(", "))
  }
}
