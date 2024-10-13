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
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B ={
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
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length <= 1) {
      if (v.isEmpty) m.zero else f(v(0))
    } else {
      val mid = v.length / 2
      val (left, right) = v.splitAt(mid)
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
    def op(a1: Par[A], a2: Par[A]): Par[A] =
      Par.map2(a1, a2)(m.op)

    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v:IndexedSeq[A], m:Monoid[B])(f: A => B): Par[B] = {
    // 입력 시퀀스의 길이가 1 이하면, 단순히 그 원소에 f를 적용하고 Par.unit으로 감쌈
    if(v.length <= 1){
      Par.unit(
        if(v.isEmpty) m.zero
        else f(v(0))
      )
    } else { // 사퀴스를 반으로 나누어 parMap을 호출하고, fork
      val (left,right) = v.splitAt(v.length / 2)
      Par.map2(
        Par.fork(parFoldMap(left, m)(f)),
        Par.fork(parFoldMap(right, m)(f))
      )(m.op)
    }
  }

  /**
   * 연습문제 10.9
   * 어려움: foldMap을 이용해서 주어진 IndexedSeq[Int]가 정렬 되어있는지 점검하라.
   * 독창적인 Monoid를 고안해야 할 것이다.
   */

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val orderMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      def zero: Option[(Int, Int, Boolean)] = None
      def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a, b) match {
        case (Some((x1, y1, o1)), Some((x2, y2, o2))) => Some((x1, y2, o1 && o2 && y1 <= x2))
        case (Some((x, y, o)), None) => Some((x, y, o))
        case (None, Some((x, y, o))) => Some((x, y, o))
        case (None, None) => None
      }
    }

    foldMapV(ints, orderMonoid)(i => Some((i, i, true))).forall(_._3)
  }

}
