package chap12

trait Monad_v2[F[_]] extends Applicative [F]{
  // Monad의 최소한의 구현은 반드시 unit을 구현해야하며
  // flatMap또는 join과 map을 재정의 해야한다.
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](faa: F[F[A]]): F[A] = flatMap(faa)(fa => fa)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
}
