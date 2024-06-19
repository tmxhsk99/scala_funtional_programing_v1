/**
 *  sealed : 이 파일 안에서 모든 로직을 구현 해야 한다는 의미
 *  trait : 특질 표시 자료 구조를 만들기 위한 특성 키워드 : list, stack, queue, tree 다양한 자료 구조 만들기 가능
 *  List[+A] : 이 자료구조 List는 공변적이라는 의미 : List[Animal] 은 Cat , Dog 와 같은 서브타입도 함께 넣기 가능하다는 뜻
 */
sealed trait List[+A]
case object Nil extends List[Nothing] // 빈 목록을 나타내는 List 자료 생성자
case class Cons[+A](head: A, tail: List[A]) extends List[A] // 비지 않은 목록을 나타내는 또 다른 자료 생성자. tail은 또 다른 List[A]로, Nil일 수도 있고 다른 Cons일 수도 있다.

/**
 * List 동반(companion) 객체, 목록의 생성과 조작을 위한 함수들을 담는다.
 */
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0 // 빈 목록의 합은 0.
    case Cons(x, xs) => x + sum(xs) // x로 시작하는 목록의 합은 x 더하기 목록 나머지 부분의 합이다.
  }

  /**
   * 패턴 매칭을 이용해 리스트가 Double인 경우 사용
   */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0 // 빈 목록의 곱은 1.0
    case Cons(0.0, _) => 0.0 // 첫 요소가 0.0이면 결과는 0.0
    case Cons(x, xs) => x * product(xs) // x로 시작하는 목록의 곱은 x 곱하기 목록 나머지 부분의 곱이다.
  }

  /**
   * 가변 인수를 받아 손쉽게 리스트를 생성하게 함
   */
  def apply[A](as: A*): List[A] = // 가변 인수 함수 구문
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // 비어 있지 않으면 첫 요소와 나머지 요소들로 Cons 생성
}