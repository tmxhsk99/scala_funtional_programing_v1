package chap12

import chap11.Functor

trait Applicative[F[_]] extends Functor[F] {

  // 기본수단 조합기들
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // 파생된 조합기들
  // map을 unit과 map2를 이용해서 구현할 수 있다.
  // () Unit 형식의유일한 값임을 기억하기 바란다. 따라서 unit(())는 그러한 가짜 값 () 로 unit을 호출하는 것에 해당한다.
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))


  // traverse의 정의는 이전과 동일한다.
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))


  // foldRight( 초기값 : unit(List()) = F[] // 빈 리스트르 F로 감싼것)(적용할 함수)
  // fas.foldRight(초기값)((현재값, 누적값) => 결과)
  // fa: 현재 처리 중인 요소 (F[A] 타입)
  // acc: 지금 까지 누적된 결과 F[List[A]] 타입
  // map2는 (fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  // 이므로 fa와 acc가 (_::_) 는 리스트에서 새로운 요소를 리스트에 앞에 추가 하는 연산자
  //요소   ::   리스트
  //↓           ↓
  // 1    ::   [2,3]   =  [1,2,3]
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight( unit(List[A]()) )( (fa, acc) => map2(fa, acc)(_ :: _) )


  // replicateM 은 F[A]의 map2연산을 n번 반복해서 그 결과를 리스트로 모아서 반환한다
  // F[A] map2 구현 방식에 따라 달라진다.
  def replicateM[A](n: Int, fa:F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // 두 F[A], F[B]타입을 F[(A,B)]형식의 튜플로 합친다.
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a,b) => (a, b))



}
