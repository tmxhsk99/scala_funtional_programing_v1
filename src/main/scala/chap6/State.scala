package chap6

import scala.collection.immutable.List

/**
 * State 클래스: 상태 S와 결과 A를 가지는 상태 변환을 표현
 */
case class State[S, +A](run: S => (A, S)) {
  /**
   * map : 결과 A를 변환하여 새로운 State를 생성
   * 매개변수 f: 현재 결과 A를 새로운 타입 B로 변환하는 함수
   * 로직: flatMap을 사용하여 현재 State의 결과에 f를 적용하고, 그 결과로 새 State를 생성
   */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  /**
   * map2 : 두 State를 조합하여 새로운 State를 생성
   * 매개변수 sb: 두 번째 State
   * 매개변수 f: 두 State의 결과를 조합하는 함수
   * 로직: 첫 번째 State의 결과를 flatMap으로 얻고, 두 번째 State의 결과를 map으로 얻어 f로 조합
   */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  /**
   * flatMap: State 변환을 연결
   * 매개변수 f: 현재 결과 A를 바탕으로 새로운 State[S, B]를 생성하는 함수
   * 로직: 현재 State를 실행하여 결과와 새 상태를 얻고, 그 결과로 f를 호출하여 새 State를 생성 및 실행
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  // Rand 타입 별칭 : RNG를 상태로 사용하는 State
  type Rand[A] = State[RNG, A]

  /**
   * unit: 주어진 값 a를 포함하는 State 생성
   * 매개변수 a: 새 State에 포함될 결과 값
   * 로직: 주어진 값 a와 입력 상태 s를 그대로 반환하는 새 State 생성
   */
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  /**
   * sequence: State 리스트를 단일 State로 변환
   * 매개변수 fs: State들의 리스트
   * 로직: 리스트의 각 State를 순차적으로 실행하고 결과를 리스트로 조합
   */
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // sequenceViaFoldLeft: sequence의 다른 구현방식
  // 매개변수 l: State들의 리스트
  // 로직: sequence와 동일하지만 foldLeft를 사용하여 구현
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  // modify: 상태 S를 변경하는 State 생성
  // 매개변수 f: 현재 상태를 새로운 상태로 변환하는 함수
  // 로직: 현재 상태를 얻고(get), 그 상태에 f를 적용한 후 새로운 상태로 설정(set)
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // get : 현재 상태를 얻는 State
  // 로직: 현재 상태를 그대로 결과로 반환하고 상태도 변경하지 않음
  def get[S]: State[S, S] = State(s => (s, s))

  // set : 새로운 상태를 설정 하는 State
  // 매개변수 s: 새로 설정할 상태
  // 로직: 주어진 s를 새로운 상태로 설정하고, 의미 있는 결과 값 없음을 나타내는 ()를 반환
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}