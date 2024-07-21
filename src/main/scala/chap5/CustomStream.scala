package chap5

sealed trait Stream[+A] {
  /**
   * 머리 추출 함수 패턴 매칭 사용 버전
   */
  def headOptionPatternMatch: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * Stream의 요소들 전체를 평가를 강제로 한다음 에 List형식으로 변환하여 반환
   * 무한 스트림 변경시 주의 필요: 프로그램이 종료안될 수 있음
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match { //
      case Empty => acc.reverse // 스트림의 리스트를 전부 평가해서 없는 경우 지금까지의 리스트를 역순으로 바꿔서 반환 (머리부터 들어갔기때문에 원래스트림의 역순이됨)
      case Cons(h, t) => go(t(), h() :: acc) // 리스트 머리에 Stream 머리의 평가 결과를 넣어서 리스트 생성하여 go 함수로 재귀호출
    }

    go(this, List()) //최초의 호출시 자기자신스트림과 빈리스트를 넣어 호출
  }

  /**
   * Stream의 처음 n개 요소를 돌려주는 함수 take
   * n > 1인 경우 : 현재 요소를 유지하고 나머지 스트림에서 n-1개를 더가져온다.
   * n == 1 인경우 : 현재 요소만을 포함하는 새스트림을 반환한다.
   * n <= 0 이거나 빈스트림 인 경우 : 빈스트림 반환
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => CustomStream.cons(h(), t().take(n - 1))
    case Cons(h, _) if (n == 1) => CustomStream.cons(h(), CustomStream.empty)
    case _ => CustomStream.empty
  }

  /**
   * Stream의 처음 n개 요소를 건너뛴 스트림을 돌려주는 drop
   * n > 0 이고 스트림이 비어있지 않은 경우 : 첫요소를 버리고 나머지 스트림에서 n-1개를 더버림
   * n <= 0 이거나 스트림이 비어있는 경우: 현재 스트림을 그대로 반환한다.
   */
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * 패턴매칭 사용버전
   * Stream에서 주어진 술어를 만족하는 선행요소들을 돌려주는 takeWhile
   * 1. 스트림의 각 요소에대해
   *  - 만약에 주어진요소가 p에 대해서 만족할때 스트림에 포함 하여 takWhile 재귀호출
   *  - 조건을 만족하지 않으면 반환 처리
   *    2. 빈스트림은 빈스트림 반환
   */
  def takeWhilePattern(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => CustomStream.cons(h(), t().takeWhilePattern(p))
    case _ => CustomStream.empty
  }

  /**
   * Stream의 모든 요소가 술어(boolean 조건문)을 만족하는지 점검하는 forAll 함수
   * 만족하지 않는 값을 만나면 즉시 순회 종료
   */
  def forAll(p: A => Boolean): Boolean = this match {
    case _ => true
    case Cons(h, t) =>
      if (p(h())) t().forAll(p)
      else false
  }

  /**
   * 패턴 매칭을 통한 exists
   */
  def existsPatternMatch(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().existsPatternMatch(p)
    case _ => false
  }

  /**
   * foldRight를 사용하여 재사용성이 증가한 exists
   */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)


  /**
   * foldRight : 데이터 구조를 오른쪽에서 왼쪽으로 접는(fold) 고차 함수이다.
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
   * foldRight를 이용해서 takeWhile 구현하기
   * 이건 못품
   */

  /**
   * 1. foldRight를 사용한다, 초기값은 빈스트림 (CustomStream.empty[A])이다.
   * 2. foldRight에 전달되는 함수 (h,t) => ...는 각 요소에 대해 다음을 수행한다.
   *    - h 현재요소
   *    - t는 나머지 스트림을 처리한 결과이다 (아직 평가되지 않은 상태)
   *      3. 만약 현재요소 h가 조건 p를 만족하면 (if (p(h))):
   *     - 새로운 스트림을 생성한다 CustomStream.cons(h, t)
   *     - 이는 현재요소 h를 스트림의 헤드로, t(나머지 처리 결과)를 테일로 하는 새스트림이다.
   *       4. 조건을 만족하지 않으면 즉시 빈스트림을 반환. (CutomStream.empty).
   *       5. foldRight 나태성 덕분에, 조건을 만족하지 않는 요소를 만나면 평가를 진행하지 않음
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(CustomStream.empty[A])((h, t) =>
      if (p(h)) CustomStream.cons(h, t)
      else CustomStream.empty
    )


  /**
   * foldRight를 사용해서 headOption 구현하기
   * 이것도 못 풀겠음
   */

  /**
   * 1. foldRight를 사용한다. 초기값을 None:Option[A]이다.
   *  - 이는 스트림이 비어있을 경우 반환될 값이다.
   *    2. foldRight에 전달되는 함수는 (h,_) => Some(h)이다:
   *  - h는 현재요소
   *  - _ 는 나머지 결과는 무시한다는 의미, (첫번째 요소만 필요하기 때문이다.)
   *    3. 이 함수의 스트림의 첫번째 요소를 만나면 즉시 Some(h)를 반환한다.
   *    4. foldRight의 나태성으로 그 이후의 평가를 하지 않음
   */
  def headOption: Option[A] =
    foldRight(None: Option[A])(
      (h, _) => Some(h)
    )

  /**
   * foldRight를 이용해서 map, filter, append, flatMap을 구현하라
   * append 메서드는 자신의 인수에 대해 엄격하지 않아야한다.
   */

  /**
   * map: 스트림의 각 요소에 함수 f를 적용한 새로운 스트림을 반환한다.
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(CustomStream.empty[B])(
      (h, t) => CustomStream.cons(f(h), t)
    )

  /**
   * filter: 주어진 조건 p를 만족하는 요소만으로 이루어진 새로운 스트림을 반환한다.
   */

  /**
   * 1. foldRight를 사용하여 스트림을 오른쪽에서 왼쪽으로 접는다.
   *
   * 2. 초기값으로 CustomStream.empty[A]를 사용한다.
   *
   * 3. foldRight에 전달되는 함수는 (h,t) => ... 이다
   *    - h는 현재 요소이다.
   *    - t는 나머지 스트림을 filter한 결과이다.(평가 되지 않은 상태)
   *      4. 각요소 h에 대해:
   *    - 만약 p(h)가 true라면, h를 새스트림에 포함시킨다.
   *      (CustomStream.cons(h,t))
   *    - 그렇지 않다면, h를 무시하고 나머지 결과 t만 반환한다.
   *
   */
  def filter(p: A => Boolean): Stream[A] =
    foldRight(CustomStream.empty[A])(
      (h, t) =>
        if (p(h)) CustomStream.cons(h, t)
        else t
    )

  /**
   * append : 현재 스트림뒤어 주어진 스트림 s 를 붙인 새로운 스트림을 반환한다.
   * 인수 s는 업격하지 않아야한다.
   */

  /**
   * 1.foldRight를 사용하여 현재 스트림을 오른쪽에서 왼쪽으로 접는다.
   * 2. 초기값으로 s를 사용한다. 이는 추가하려는 스트림이다.
   *    - s는 인자로 전달된 지연평가 스트림이다.
   *      foldRight에 전달되는 함수는 (h, t) => CustomStream.cons(h, t) 입니다:
   *
   * h는 현재 요소입니다.
   * t는 나머지 결과입니다 (현재 스트림의 나머지 부분과 s가 합쳐진 상태).
   *
   * 각 요소 h에 대해 새로운 Cons 노드를 만들어 현재 요소를 스트림의 앞부분에 추가합니다.
   */
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)(
      (h, t) => CustomStream.cons(h, t)
    )

  /**
   * flapMap: 각 요소에 함수 f를 적용하고, 그 결과로 나온 모든 스트림을 하나의 스트림으로 평탄화한다.
   */

  /**
   * 1. foldRight를 사용하여 스트림을 오른쪽에서 왼쪽으로 접는다.
   * 2. 각요소 h에 대해:
   *    - f(h)를 호출하여새로운 스트림을 생성한다.
   *    - 이 새스트림을 t(나머지 결과)에 append한다.
   *      3. append 메서드는 이전구현한대로 두스트림을 효율적으로 연결한다.
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(CustomStream.empty[B])((h, t) => f(h) append t)

  /**
   * n에서 n+1, n+2 등으로 이어지는 무한 정수 스트림을 생성하는 함수
   */
  def from(n: Int): Stream[Int] =
    CustomStream.cons(n, from(n + 1))

  /**
   * 무한 피보나치 수 0,1,1,2,3,5,8 ... 으로 이루어진 무한 정수 스트림을 생성하는 함수를 작성하라
   */
  def fibs (n:Int): Stream[Int] = {
    def fibsHelper(a: Int, b: Int): Stream[Int] =
      CustomStream.cons(a, fibsHelper(b, a + b))

    fibsHelper(0, 1)
  }

  /**
   *  일반화된 스트림 구축 함수 unfold를 작성하라
   *  초기 상태 하나와 다음 상태 및 다음값(생성된 스트림 안의)을 산출하는 함수를 하나 받아야한다.
   *  A: 생성될 스트림의 타입
   *  S: 상태의 타입
   *
   *  z: 초기 상태
   *  f: S => Option[(A,S)] : 상태를 입력 으로 받아 옵션을 반환 하는 함수
   */
  def unfold [A, S](z:S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => CustomStream.cons(a, unfold(s)(f))
      case None => CustomStream.empty
    }
  }

}


case object Empty extends Stream[Nothing]

/**
 * 비지 않은 스트림은 하나의 머리와 하나의 꼬리로 구성된다.
 * 둘 다 엄격하지 않은 값인데, 기술적인 한계 때문에
 * 이 들은 이름으로 전달되는 인수가 아니라 반드시 명시적으로 강제해야 하는 성크이다.
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object CustomStream {
  /**
   * 비지 않은 스트림 생성을 위한 똑똑한 생성자.
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd // 평가 반복을 피하기 위해 head와 tail을 게으른 값으로서 캐싱을 피한다.
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * 특정 형식의 빈스트림을 생성하기 위한 똑똑한 생성자
   */
  def empty[A]: Stream[A] = Empty

  /**
   * 여러 요소로 이루어진 Stream의 생성을 위한 편의용 가변 인수 메서드
   */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


}
