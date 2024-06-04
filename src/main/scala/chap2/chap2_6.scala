package chap2

/**
 * 챕터 2.6 형식 에서 도출된 구현
 */
object chap2_6 {
  //두 수를 더하는 함수
  val add = (a: Int, b: Int) => a + b

  // 부분 적용 함수 partial1 함수 정의
  /*
   */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  // 부분 적용 함수 중괄호 없는 버전
  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // 부분 적용 함수 중괄호 없음 + 형식 주해 생략 버전
  def partial3[A, B, C](a: A, f: (A, B) => C): B => C = (b) => f(a, b)

  def main(args: Array[String]): Unit = {
    // [A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)
    // [A, B, C]( 3 , add: (A, B) => C): B => C = (b: B) => add (3 , b)
    val addThree = partial3(3, add)
    // [A, B, C]( 3 , add: (A, B) => C): B => C = (b: B) => add (3 , b)
    // [A, B, C]( 3 , add: (A, B) => C): B => C = (4) => add (3 , 4)
    val addThreeAndFour = addThree(4)
    // add (3 , 4) => 3 + 4
    // 결과 7
    println(addThreeAndFour)
  }
}


