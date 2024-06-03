package chap2

/**
 * 연습 문제 2.3
 * 또 다른 예로, 인수가 두개인 함수 f를 인수 하나를 받고 그것으로 f를 부분적용하는 함수로 변환하는
 * 커링을 살펴 보자.
 * 이번에도 컴파일되는 구현은 단 한가지 이다.
 * def curry[A,B,C](f: (A, B) => C) : A => (B => C)
 */
object Practice_2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def main(args: Array[String]): Unit = {
    // 두 수를 더하는 함수
    val add = (a: Int, b: Int) => a + b

    // add 함수를 커링 함수화
    // 최초 형태의 함수 (a: A) => (b: B) => f(a, b)
    // 적용 후 :(a: A) => (b: B) => (a:Int, b:Int) => a + b
    val curriedAdd = curry(add);

    // 커링된 함수를 사용 하여 부분 적용
    // 부분 적용 전 : (a: A) => (b: B) => (a:Int, b:Int) => a + b
    // 적용 후 : (5) => (b: B) => (5, b) => 5 + b
    val addFive = curriedAdd(5)

    // 부분 적용 전 : (b: B) => (5, b) => 5 + b
    // 적용 후 : (10) => (5, 10) => 5 + 10
    val result = addFive(10)
    println(result)

  }

}
