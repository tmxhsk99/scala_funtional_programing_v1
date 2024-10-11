package chap10

import answer.Prop.forAll
import answer.{Gen, Prop}

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

}