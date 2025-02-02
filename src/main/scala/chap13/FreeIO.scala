package chap13

import chap11.Monad

object FreeIO {

  /*
  우리는 'TailRec'과 'Async'를 'Free' 타입으로 일반화할 수 있습니다.
  이는 어떤 F[_]에 대해서도 Monad가 될 수 있습니다.
  */

  /**
   * Free Monad란?
   * Free Monad는 어떤 타입 F[_]에 대해서도 Monad로 만들 수 있는 구조입니다.
   * 이는 순수 함수형 프로그래밍에서 부수 효과(side effects)를 다루는 강력한 방법입니다.
   */

  sealed trait Free[F[_],A] { // Free Monad의 기본 trait
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[F[_],A](a: A) extends Free[F, A] // 순수 값을 감싸는 케이스
  case class Suspend[F[_],A](s: F[A]) extends Free[F, A]  // F 타입의 계산을 감싸는 케이스
  case class FlatMap[F[_],A,B](s: Free[F, A],
                               f: A => Free[F, B]) extends Free[F, B] // 연속된 계산을 표현하는 케이스

  // Exercise 1: Implement the free monad
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F,a]})#f] {
      def unit[A](a: => A) = Return(a)
      def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
    }

  /**
   * Function0 타입에 대한 특별한 인터프리터입니다.
   * 스택 안전성(stack safety)을 보장하면서 계산을 실행합니다
   */
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x,f) => x match {
      case Return(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

  // 어떤 Monad에 대해서도 작동하는 일반적인 인터프리터입니다.
  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  //  계산을 정규화된 형태로 변환합니다.
  // (오른쪽으로 결합된 FlatMap, Suspend, 또는 Return 중 하나로)
  @annotation.tailrec
  def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  /**
   * 사용 목적:
   *
   *
   * 재귀적인 계산을 스택 안전하게 만들기
   * 부수 효과가 있는 계산을 순수하게 표현하기
   * 계산의 설명(description)과 실행(execution)을 분리하기
   */
}
