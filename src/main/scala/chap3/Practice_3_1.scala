import scala.annotation.tailrec

/**
 * sealed : 이 파일 안에서 모든 로직을 구현 해야 한다는 의미
 * trait : 특질 표시 자료 구조를 만들기 위한 특성 키워드 : list, stack, queue, tree 다양한 자료 구조 만들기 가능
 * List[+A] : 이 자료구조 List는 공변적이라는 의미 : List[Animal] 은 Cat , Dog 와 같은 서브타입도 함께 넣기 가능하다는 뜻
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


  /**
   * 연습문제 3.2 리스트의 첫 요소를 제거하는 tail을 구현
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil // 빈 리스트 인 경우 빈 리스트 반환
    case Cons(_, xs) => xs // 첫 요소를 제거 하고 나머지 리스트 반환
  }

  /**
   * 연습문제 3.3 리스트의 첫 요소를 다른값으로 대체하는 함수 setHead의 구현
   */
  def setHead[A](list: List[A], head: A) = list match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  /**
   * 연습문제 3.4
   * tail을 일반화해서, 목록에서 처음 n개의 요소들 제거하는 함수 drop을 구현하라.
   * 이 함수의 실행시간은 제거ㅏ되는 원소의 개수에만 비례함에 주의할 것
   * List 전체의 복사본을 만들 필요는 없다.
   * def drop[A](l: List[A], n:Int): List[A]
   */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil // 리스트가 비어 있으면 빈 리스트 반환
    case (xs, 0) => xs // n이 0이면 리스트 그대로 반환
    case (Cons(_, xs), n) => drop(xs, n - 1) // 첫 요소를 제거하고 n을 감소시킨 후 재귀 호출
  }

  /**
   * 연습문제 3.5 주어진 술어(predeicate)와 부합하는 List의 앞 요소(prefix)를 제거하는
   * 함수 dropWhile을 구현하라.
   */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  /**
   * 연습문제 3.6
   * 그러나 모든 것이 효율적이지는 않다.
   * 한 List의 마지막 요소를 제외한 모든 요소로 이루어진 List를 돌려주는 함수
   * init을 구현하라.
   * 예를 들어 List(1,2,3,4)에 대해 init은 List(1,2,3)을 돌려주어야 한다.
   * 이 함수를 tail 처럼 상수시간을 구현할 수 없는 이유는 무엇일까?
   * 마지막 요소를 제외 하려면 마지막까지 가야되기 때문이다 ^^
   */
  def init[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("빈 리스트 입니다.")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   * 목록 3.2 오른쪽 접기 함수의 간단한 용례
   */

  /**
   * 리스트의 모든 요소에 대해 오른쪽에서 왼쪽으로 함수 f를 적용하여 단일 값으로 축약하는 함수.
   * @param as : 다형 타입 A의 리스트
   * @param z  : 빈 목록일 경우 반환할 기본값 (초기값)
   * @param f  : 두 인수를 받아 하나의 값으로 합치는 함수. 첫 번째 인수는 리스트의 요소, 두 번째 인수는 축약된 값.
   * @tparam A : 리스트 요소의 타입
   * @tparam B : 축약된 결과의 타입
   * @return   : 리스트의 모든 요소에 f를 적용하여 얻은 단일 값
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) //_ * _ 은 (x,y) => x * y 를 좀 더 간결하게 표기한 것이다.

  /**
   * 연습문제 3.7
   * foldRight로 구현된 product(목록3.2의 product2)가
   * 0.0을 만났을 때 즉시 재귀를 멈추고 0.0을 돌려줄까?
   * 왜 그럴까? 아니라면 왜 아닐까?
   *
   * 아님 -> 재귀 구현이기 때문에 리스트 마지막 까지 돔
   *
   * foldRight 를 긴 목록으로 호출 했을 때 어떤 평가 단축이 어떤 식으로 일어나는지 고찰하라.
   * 이는 다른 연습 문제 들보다 심오한 문제 이다.
   *
   * 내가 이해한 내용 :
   * 강제인수 비강제인수
   * 기본적으로 모든지 바로 평가된다.
   * 지연평가는 평가가 필요할때 까지 미룬다.
   * 그러므로 실제 매개변수상에서 평가하는거랑 그냥 인수상 에서 평가하는거랑
   * 함수 내에서 평가하는거랑 의미가 달라진다.
   */

  /**
   * 강제 인수 : 즉시 평가(지연 평가 하지 않음 인수상에서 평가가끝난후 재귀실행)
   */
  // 강제 인수를 사용하는 foldRight
  def foldRightStrict[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) =>
        println(s"즉시 평가된 foldRight 의 x = $x")
        f(x, foldRightStrict(xs, z)(f))
    }

  def productStrict(ns: List[Double]): Double =
    foldRightStrict(ns, 1.0) { (x, y) =>
      println(s"즉시 평가 조건: x = $x, y = $y")
      if (x == 0.0) 0.0
      else x * y
    }



  /**
   * 비강제 인수 : 지연평가 (재귀상에서 평가하므로 재귀중 평가)
   */
  // 비강제 인수를 사용하는 nonStrictfoldRight
  def nonStrictFoldRight[A, B](as: List[A], z: => B)(f: (A, => B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) =>
        f(x, nonStrictFoldRight(xs, z)(f))
    }

  def productNoneStrict(ns: List[Double]): Double =
    nonStrictFoldRight(ns, 1.0)((x, y) => {
      println(s"지연 평가 조건: x = $x")
      if (x == 0.0) 0.0 else {
        println(s"평가된 y for x = $x")
        x * y
      }
    })
  def main(args: Array[String]): Unit = {

  }

  /**
   * 연습문제 3.8
   * foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_))
   * 처럼 Nil과 Cons자체를 foldRight에 전달하면 어떤일이 발생할까?
   * -> 리스트가 복사된
   *
   * 그냥 순회하면서 전달된 함수를 실행하는 것뿐이니 함수가 Cons라면
   * 새로운 요소를 순회하며 새로운 리스트를 만들 뿐이다.
   */

  /**
   * 연습문제 3.9
   * foldRight를 이용해서 목록의 길이를 계산하라
   */
  def lenght[A](as: List[A]):Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  /**
   * 연습문제 3.10
   * 이번 절의 foldRight 구현은 꼬리 재귀가 아니므로 긴 목록에 대해서는
   * StackOverflowError 오류가 발생한다.
   * 실제로 실험 및 일반적인 목록 재귀함수 foldLeft를 이전 장에서 논의한 기법들을 이용해서 작성하라.
   * 서명은 다음과 같다.
   */

  /**
   *
   * 왼쪽에서 오른쪽으로 리스트의 요소를 처리하여 누적된 값을 계산하는 함수입니다.
   *
   * @param as 처리할 리스트입니다. 이 리스트의 각 요소는 함수 f에 의해 처리됩니다.
   * @param z 초기값입니다. 리스트의 첫 번째 요소를 처리하기 전에 이 값으로 시작합니다.
   * @param f 누적된 값과 리스트의 현재 요소를 받아 새로운 누적 값을 반환하는 함수입니다.
   * @tparam A 리스트 요소의 타입입니다.
   * @tparam B 결과 값의 타입입니다.
   * @return 리스트의 모든 요소를 처리한 후 최종 누적 값을 반환합니다.
   */
  def foldLeft[A,B](as: List[A],z: B)(f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(z,h))(f)
  }

  /**
   * foldRight 와 foldLeft 의 차이
   *
   * foldRight(List(1, 2, 3), 0)(_ + _)
   * // => 1 + foldRight(List(2, 3), 0)(_ + _)
   * // => 1 + (2 + foldRight(List(3), 0)(_ + _))
   * // => 1 + (2 + (3 + foldRight(Nil, 0)(_ + _)))
   * // => 1 + (2 + (3 + 0))
   *
   * 와 같이 1 + 재귀함수 이므로 최초 앞의 연산을 위해 스택에 담아서 진행해야 한다.
   *
   * foldLeft(List(1, 2, 3), 0)(_ + _)
   * // => foldLeft(List(2, 3), 1)(_ + _)
   * // => foldLeft(List(3), 3)(_ + _)
   * // => foldLeft(Nil, 6)(_ + _)
   * // => 6
   */



  /**
   * 효율성 손실의 한 예로, List가 또 다른 List를 순차열로서 담고잇는지 점검하는
   * headSubsequence 함수를 구현하라.
   * 예를 들어 List(1,2)나 List(2,3), List(4)는
   * List(1,2,3,4)의 부분 순차열 이다.
   * 간결하고 순수 함수로만 이루어진, 그러면서도
   * 효율적인 구현을 고안하기 어려울 수 있겠지만
   * 개의치않고 자연스러운 방법으로 구현할것
   * def hasSubSeqeunce[A](sup: List[A], sub: List[A]):Boolean
   */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}