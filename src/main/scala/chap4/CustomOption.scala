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
  // 스칼라 표준라이브러리 List 호환을 위해 apply 추가
  def apply[A](opt: scala.Option[A]): Option[A] = opt match {
    case scala.Some(value) => Some(value)
    case scala.None => None
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 시퀸스의 평균을 계산하는 mean 함수
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // 실전 사용 예시를 위한 클래스
  case class User(id: Int, name: String, email: Option[String], age: Option[Int])

  // 사용자 데이터베이스 (간단한 예시를 위해 List 사용)
  private val users = List(
    User(1, "Alice", Some("alice@example.com"), Some(30)),
    User(2, "Bob", None, Some(25)),
    User(3, "Charlie", Some("charlie@example.com"), None)
  )

  // ID로 사용자를 찾는 함수
  def findUserById(id: Int): Option[User] =
    CustomOption(users.find(_.id == id))

  // 사용자의 이메일을 가져오는 함수
  def getUserEmail(id: Int): Option[String] =
    findUserById(id).flatMap(_.email)

  // 사용자의 나이를 가져오는 함수
  def getUserAge(id: Int): Option[Int] =
    findUserById(id).flatMap(_.age)

  // 사용자의 나이가 특정 값 이상인지 확인하는 함수
  def isUserAdult(id: Int, adultAge: Int): Option[Boolean] =
    getUserAge(id).map(_ >= adultAge)

  // 사용자의 이메일 도메인을 가져오는 함수
  def getUserEmailDomain(id: Int): Option[String] =
    getUserEmail(id).map(_.split("@").last)

  def main(args: Array[String]): Unit = {
    // 예시 1: 존재하는 사용자의 이메일 가져오기
    println(getUserEmail(1)) // Some(alice@example.com)

    // 예시 2: 존재하지 않는 사용자의 이메일 가져오기
    println(getUserEmail(4)) // None

    // 예시 3: 이메일이 없는 사용자의 이메일 가져오기
    println(getUserEmail(2)) // None

    // 예시 4: 사용자가 성인인지 확인 (성인 나이를 18세로 가정)
    println(isUserAdult(1, 18)) // Some(true)
    println(isUserAdult(2, 18)) // Some(true)
    println(isUserAdult(3, 18)) // None (나이 정보가 없음)
    println(isUserAdult(4, 18)) // None (사용자가 존재하지 않음)

    // 예시 5: 사용자의 이메일 도메인 가져오기
    println(getUserEmailDomain(1)) // Some(example.com)
    println(getUserEmailDomain(2)) // None (이메일 정보가 없음)
    println(getUserEmailDomain(4)) // None (사용자가 존재하지 않음)

    // 예시 6: getOrElse 사용하여 기본값 제공
    println(getUserAge(1).getOrElse("나이 정보 없음")) // 30
    println(getUserAge(3).getOrElse("나이 정보 없음")) // 나이 정보 없음

    // 예시 7: orElse 사용하여 대체 Option 제공
    println(getUserEmail(2).orElse(Some("이메일 없음"))) // Some(이메일 없음)

    // 예시 8: filter 사용하여 조건에 맞는 경우만 선택
    println(getUserAge(1).filter(_ > 20)) // Some(30)
    println(getUserAge(2).filter(_ > 30)) // None
  }
}