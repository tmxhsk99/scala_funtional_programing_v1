package chap11

import answer.Gen
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
   */
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  /**
   * List 모나드
   */
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List.apply(a)

    override def flatMap[A, B](ma: scala.List[A])(f: A => scala.List[B]): scala.List[B] = ma flatMap f
  }
}





