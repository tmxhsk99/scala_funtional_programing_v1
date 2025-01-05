package chap12

import chap11.Functor

/**
 * Applicative 를 Unit과 Apply를 기본으로 구현해보기
 */
trait Applicative_v2[F[_]] extends Functor[F]{

  def apply[A,B](fab:F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // apply와 unit을 이용한 map 구현
  // 1. f: A => B 를 unit으로 감싸서 F[A => B] 타입으로 만든다.
  // 2. 그 다음 apply 사용해서  F[A]에 적용한다.
  // 3. 결과적으로 F[B]를 얻는다.
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // apply와 unit을 이용한 map2 구현
  // 1. f.curried 통해 이항 함수를 커리된 형태로 변환한다. ex) (A,B) => C 를 A => (B => C) 로 변환한다.
  // 2. map(fa)(f.curried)를 통해 F[B => C] 타입을 얻는다.
  // 3. 이를 apply를 통해 fb: F[B]에 적용하여 F[C]를 얻는다.
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // 반대로 map2와 unit을 이용한 apply 구현
  // 1. map2를 이용해 F[A => B]와 F[A]를 결합한다.
  // 2. 결합 함수로는 단순히 함수 적용((f, a) => f(a))을 사용한다.
  def apply2[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f,a) => f(a))
}
