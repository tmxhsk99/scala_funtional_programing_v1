package chap5

sealed trait Stream[+A] {
  /**
   * 머리 추출 함수
   */
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, List())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => CustomStream.cons(h(), t().take(n - 1))
    case Cons(h, _) if (n == 1) => CustomStream.cons(h(), CustomStream.empty)
    case _ => CustomStream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => CustomStream.cons(h(), t().takeWhile(p))
    case _ => CustomStream.empty
  }

}

case object Empty extends Stream[Nothing]

/**
 * 비지 않은 스트림은 하나의 머리와 하나의 꼬리로 구성된다.
 * 둘 다 엄격하지 않은 값인데, 기술적인 한계 때문에
 * 이 들은 이름으로 전달되는 인수가 아니라 반드시 명시적으로 강제해야 하는 성크이다.
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object CustomStream {
  /**
   * 비지 않은 스트림 생성을 위한 똑똑한 생성자.
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd // 평가 반복을 피하기 위해 head와 tail을 게으른 값으로서 캐싱을 피한다.
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /**
   * 특정 형식의 빈스트림을 생성하기 위한 똑똑한 생성자
   */
  def empty[A]: Stream[A] = Empty

  /**
   * 여러 요소로 이루어진 Stream의 생성을 위한 편의용 가변 인수 메서드
   */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


}
