package chap2

/**
 *
 * curry의 변환을 역으로 수행하는 고차함수 uncurry 를 구현하라. =>는 오른쪽으로 묶이므로,
 * A => (B => C) 를 A => B => C 라고 표기할 수 있음을 주의할 것.
 *
 * def uncurry[A,B,C] (f: A => B => C): (A, B) => C
 *
 * A => (B => C) 를  (A, B) => C 로 바꾼다.
 */
object Practice_2_4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    // 두 수를 더하는 커링된 함수
    val curriedAdd = (a: Int) => (b: Int) => a + b

    // curriedAdd 함수를 언커링
    // (f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
    // (a: Int) => (b: Int) => a + b : (A, B) => C = (a: A, b: B) => f(a)(b)
    val uncrriedAdd = uncurry(curriedAdd)

    // uncurriedAdd는 이제 두 개의 인수를 받아 결과를 반환
    // (a: Int) => (b: Int) => a + b : (A, B) => C = (a: A, b: B) => f(a)(b)
    // (a: Int) => (b: Int) => a + b : (A, B) => C = (5, 10) => f(5)(10)
    val result = uncrriedAdd(5,10)
    println(result)

  }
}
