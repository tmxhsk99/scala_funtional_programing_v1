package chap15


sealed trait Process[I, O] {
  def emit(head: O): Process[I, O] = Emit(head)

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      // 처리 공정 자체가 중지 되었으면 다시 시작한다.
      case Halt() => go(this)
      case Await(recv) => Await {
        // 공급원으로 부터 종료 되었다면 반복하지 않느다.
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _ => Halt()
    }.repeat
  }

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = {
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None => Halt()
      }
      go(0.0)
    }

  }
}

case class Emit[I, O](
                       head: O,
                       tail: Process[I, O] = Halt[I, O]()
                     ) extends Process[I, O]

case class Await[I, O](
                        recv: Option[I] => Process[I, O]
                      ) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

object Process {

}
