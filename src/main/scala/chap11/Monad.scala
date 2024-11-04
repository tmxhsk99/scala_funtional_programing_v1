package chap11

import answer.Gen
import answer.Prop.S
import chap6.State
import chap7.Par
import chap7.Par.Par


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribution[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

/**
 * Monad는 map의 기본 구현을 제공하므로 Functor를 확장할 수 있다.
 * 모든 모나드는 함수자이나, 모든 함수자가 모나드인 것은 아니다
 */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
   * 리스트 안의 모나드들을 하나의 모나드로 합치기
   * // List[Option[A]] -> Option[List[A]]
   * val inputs = List(Some(1), Some(2), Some(3))
   * sequence(inputs) // 결과: Some(List(1, 2, 3))
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  /**
   * 리스트의 각 요소에 모나드를 반환하는 함수를 적용하고 결과를 하나로 합치기
   * // List[A]와 A => Option[B]를 받아서 Option[List[B]] 반환
   * def divide10(x: Int): Option[Double] =
   * if (x == 0) None
   * else Some(10.0 / x)
   *
   * traverse(List(2, 5, 10))(divide10)
   * // 결과: Some(List(5.0, 2.0, 1.0))
   *
   * traverse(List(2, 0, 10))(divide10)
   * // 결과: None (0으로 나누기 시도)
   */
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))


  /**
   * replicateM : 모나드 F 안서 어떤 동작(ma)를 n번 반복하여 그 결과들의 리스트를 만드는 함수
   * 입력으로 반복 횟수 n과 모나드 값 ma를 받아서, 그 모나드 동작을 n번 수행한 결과들을 리스트로 모아 반환한다.
   */
  def replicatedM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0)
      unit(List()) // 빈리스트를 모나드로 감싸서 반환
    else
      map2(ma, replicatedM(n - 1, ma))(_ :: _) // 현재 값과 나머지 재귀 결과를 합칩

}


object Monad {
  /**
   * Gen 모나드
   */
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  /**
   * Par 모나드
   */
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

  /**
   * Option 모나드
   */
  val optionMonad = new Monad[Option] {

    def unit[A](a: => A): Option[A] = Option.apply(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  /**
   * Stream 모나드
   * Stream은 scala 버전 2.13 버전 부터는 Deprecated 되고 LazyList 사용 권장 으로 변경됨
   */
  val streamMonad = new Monad[LazyList] {
    def unit[A](a: => A) = LazyList(a)

    override def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma flatMap f
  }

  /**
   * List 모나드
   */
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List.apply(a)

    override def flatMap[A, B](ma: scala.List[A])(f: A => scala.List[B]): scala.List[B] = ma flatMap f
  }

  // 연습문제 11.2

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](ma: State[S, A])
                     (f: A => State[S, B]): State[S, B] = ma flatMap f
  }
}





