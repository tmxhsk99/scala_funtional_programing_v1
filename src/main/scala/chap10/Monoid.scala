package chap10

import answer.Prop.forAll
import answer.{Gen, Prop}
import chap7.Par
import chap7.Par.Par

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  /**
   * String 모노이드
   */
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  /**
   * 목록 모노이드
   */
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  /**
   * 연습 문제 10.1
   */

  /**
   * 정수 모노이드
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y

    def zero = 0
  }

  /**
   * 곱하기 모노이드
   */
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y

    def zero = 1
  }

  /**
   * or 모노이드
   */
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y

    def zero: Boolean = false
  }

  /**
   * and 모노이드
   */
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y

    def zero: Boolean = true
  }

  /**
   * Option 모노이드
   */

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    def zero: Option[A] = None
  }

  /**
   * 인수의 형식과 반환값 형식이 같은 함수를 자기함수(endofuntion)라고 부른다. 자기 함수들을 위한 모노이드를 작성하라
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f andThen g

    def zero: A => A = identity
  }

  /**
   * 제 2장에서 개발한 속성 기반 검사 프레임 워크를 이용해서 모노이드 법칙에 대한 속성을 구현하라. 그리고 그 속성을 이용해서 앞에서 작성한 모노이드들을 검사하라
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /**
   * 10.5 foldMap 을 구현하라
   * f함수를 적용하고, 그 결과의 zero와 op를 이용해 왼쪽에서 오른쪽으로 접는다.
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  /**
   * 10.6
   * 어려움: foldMap 함수를 foldLeft 나 foldRight 를 이용해서 구현할 수 있다.
   * 그런데 foldLeft 와 foldRight 를 foldMap 을 이용해서 구혀할 수도 있다.
   * 시도해보라
   */

  /**
   * foldLeft의 경우, 함수들을 오른쪽에서 왼쪽으로 합성한다. (compose)
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val m = new Monoid[B => B] {
      def zero: B => B = identity

      def op(f: B => B, g: B => B): B => B = f compose g
    }
    foldMap(as, m)(a => b => f(b, a))(z)
  }

  /**
   * foldRight의 경우 함수들을 왼쪽에서 오른쪽으로 합성한다. (andThen)
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val m = new Monoid[B => B] {
      def zero: B => B = identity

      def op(f: B => B, g: B => B): B => B = f andThen g
    }
    foldMap(as, m)(a => b => f(a, b))(z)
  }

  /**
   * 연습문제 10.7: Indexedseq에 대한 foldMap 구현
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) {
        m.zero
    } else if (v.length == 1) {
      f(v(0))
    } else {
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  /**
   * 연습문제 10.8
   * 어려움: 제7장에서 개발한 라이브러리를 이용해서 foldMap의 병렬 버전도 구현하라.
   * 힌트: Monoid[A]를 Monoid[Par[A]]로 승격하는 조합기 par를 구현하고,
   * 그것을 이용해서 parFoldMap을 구현할 것.
   */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = Par.unit(m.zero)

    def op(a1: Par[A], a2: Par[A]): Par[A] =
      Par.map2(a1, a2)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val par_IndexedSeq_B = Par.parMap(v)(f)
    Par.flatMap(par_IndexedSeq_B) { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }
  }

  /**
   * 연습문제 10.9
   * 어려움: foldMap을 이용해서 주어진 IndexedSeq[Int]가 정렬 되어있는지 점검하라.
   * 독창적인 Monoid를 고안해야 할 것이다.
   */

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // 정렬 상태를 추적하기 위한 모노이드 정의
    val orderMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      // 빈 시퀸스를 나타내는 초기값
      def zero: Option[(Int, Int, Boolean)] = None

      // 두 부분 시퀸스의 결과를 결합하는 연산
      def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a, b) match {
        case (Some((x1, y1, o1)), Some((x2, y2, o2))) =>
          // o1,o2: 각 부분 시퀸스의 정렬 상태
          // y1 > x2: 각 부분 시퀸스 사이의 정렬 상태 확인
          if(!o1 || !o2 || y1 > x2) { // 정렬되지 않은 상태 발견
            Some((x1,y2,false))
          } else // 여전히 정렬된 상태
            Some((x1,y2,true))
        case (Some((x, y, o)), None) => Some((x, y, o)) // 오른쪽이 빈경우, 왼쪽 유지
        case (None, Some((x, y, o))) => Some((x, y, o)) // 왼쪽이 빈 경우, 오른쪽 유지
        case (None, None) => None // 둘다 빈 경우
      }
    }

    // foldMapV를 사용하여 전체 시퀸스에 대한 정렬 상태 확인
    foldMapV(ints, orderMonoid)(i => Some((i, i, true))) match {
      case Some((_,_,isOrdered)) => isOrdered // 최종 정렬 상태 반환
      case None => true // 빈 시퀸스는 정렬된 것으로 간주
    }
  }

  // 테스트를 위한 간단한 함수
  def testOrdered(seq: IndexedSeq[Int]): Unit = {
    //println(s"Sequence: ${seq.mkString(", ")}")
    println(s"Is ordered: ${ordered(seq)}\n")
  }

  /**
   * 실제 모노이드 테스트 함수
   * @param args
   */
  def main(args: Array[String]): Unit = {
    /**
     * parFoldMap
     */
    val numbers = (1 to 1000000).toIndexedSeq
    val sumMonoid = new Monoid[Int] {
      def zero = 0
      def op(a: Int, b: Int) = a + b
    }
    // identity는 항등함수 편리 유틸용 항등함수
    // 현재는 배열을 변형하지 않고 병렬 처리만 적용하기 위해 항등함수를 적용
    // 예: 제곱의 합을 계산하려면 다음과 같이 할 수 있습니다:
    // val parSumOfSquares: Par[Int] = parFoldMap(numbers, sumMonoid)(x => x * x)
    val parSum: Par[Int] = parFoldMap(numbers, sumMonoid)(identity)

    // 실행
    import java.util.concurrent.Executors
    val es = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val sum = Par.run(es)(parSum).get()
    println(s"Sum: $sum")
    es.shutdown()


    // orderd 사용 예시
    // 예시 1: 정렬된 시퀀스
    testOrdered(IndexedSeq(1, 2, 3, 4, 5))
    // 예상 출력:
    // Sequence: 1, 2, 3, 4, 5
    // Is ordered: true

    // 예시 2: 정렬되지 않은 시퀀스
    testOrdered(IndexedSeq(1, 3, 2, 4, 5))
    // 예상 출력:
    // Sequence: 1, 3, 2, 4, 5
    // Is ordered: false

    // 예시 3: 동일한 요소를 포함한 정렬된 시퀀스
    testOrdered(IndexedSeq(1, 2, 2, 3, 4, 4, 5))
    // 예상 출력:
    // Sequence: 1, 2, 2, 3, 4, 4, 5
    // Is ordered: true

    // 예시 4: 단일 요소 시퀀스
    testOrdered(IndexedSeq(42))
    // 예상 출력:
    // Sequence: 42
    // Is ordered: true

    // 예시 5: 빈 시퀀스
    testOrdered(IndexedSeq())
    // 예상 출력:
    // Sequence:
    // Is ordered: true

    // 예시 6: 큰 정렬된 시퀀스
    testOrdered((1 to 1000000).toIndexedSeq)
    // 예상 출력:
    // Sequence: 1, 2, 3, ..., 999999, 1000000
    // Is ordered: true

    // 예시 7: 큰 정렬되지 않은 시퀀스
    testOrdered((1 to 1000000).toIndexedSeq.updated(500000, 0))
    // 예상 출력:
    // Sequence: 1, 2, 3, ..., 0, ..., 999999, 1000000
    // Is ordered: false
  }
}

