package chap12

import chap11.Functor

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_],A,B] (fa:F[A])(f: A =>G[B])(
                         implicit G: Applicative[G]) : G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fga: F[G[A]])(
                      implicit G:Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map [A,B](fa: F[A])(f: A => B): F[B] = ???

}
