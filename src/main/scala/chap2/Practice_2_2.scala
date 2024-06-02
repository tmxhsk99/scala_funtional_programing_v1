package chap2

/**
 * Array[A] 가 주워진 비교 함수에 의거 해서 정렬 되어 있는지 점검하는 isSorted 함수를 구현 하라,
 * 서명은 다음과 같다.
 */
object Practice_2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def checkSortLoop (n: Int): Boolean = {
      if(n >= as.length - 1) true
      else if(ordered(as(n),as(n+1))) checkSortLoop(n+1)
      else false
    }

    checkSortLoop(0)
  }

  def main(args: Array[String]): Unit = {
    // 정수 배열 테스트
    val intArray = Array(1, 2, 9, 4, 5)
    println(isSorted(intArray, (x: Int, y: Int) => x <= y)) // 오름차순 정렬 확인

    // 문자열 배열 테스트
    val stringArray = Array("apple", "banana", "cherry")
    println(isSorted(stringArray, (x: String, y: String) => x <= y)) // 알파벳 순서 정렬 확인
  }
}
