package chap4

sealed trait Option[+A] {

  // map 함수: 만일 Option이 None이 아니면 f를 적용한다.
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  // getOrElse 함수: B >: A는 B 형식 매개변수가 반드시 A의 상위형식(supertype)이어야 함을 의미한다.
  // Option의 Some 안의 결과르 돌려준다. 단, Option이 None이면 주어진 기본값을 돌려준다.
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  // flatMap 함수: 만일 Option이 None이 아니면 f(실패할 수 있음)를 적용한다.
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)


  // orElse 함수: ob는 필요한 경우에만 평가한다.
  // 첫 Option이 정의되어있는 경우 그것을 돌려주고 그렇지 않으면 둘째 Option을 돌려준다.
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  // filter 함수: 값이 f를 만족하지 않으면 Some을 None으로 변환한다.
  def filter(f: A => Boolean): Option[A] =
    if (map(f).getOrElse(false)) this
    else None

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

}
case class Some[+A](get: A) extends Option[A]
// 값이 있을때
case object None extends Option[Nothing] // 없는 경우


object CustomOption {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 시퀸스의 평균을 계산하는 mean 함수
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def main(args: Array[String]): Unit = {

    // map 사용 예
    println(Some(1).map(_ + 1))       // Some(2)

    // flatMap 사용 예
    println(Some(1).flatMap(x => Some(x + 1))) // Some(2)

    // getOrElse 사용예
    println(Some(1).getOrElse(2))     // 1
    println(None.getOrElse(2))        // 2

    // orElse 사용 예
    println(Some(1).orElse(Some(2)))  // Some(1)
    println(None.orElse(Some(2)))     // Some(2)

    //filter 사용 예
    println(Some(2).filter(_ % 2 == 0)) // Some(2)
    println(Some(3).filter(_ % 2 == 0)) // None

    // mean 사용 예
    val numbers = Seq(1.0, 2.0, 3.0)
    println(CustomOption.mean(numbers))     // Some(2.0)
  }
}