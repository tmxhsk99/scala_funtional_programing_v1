package chap4

sealed trait Either[+E, +A] {
  /**
   * Right 값에는 함수를 그대로 적용
   * Left 일경우 그대로 반환 (값 변경 되지 않음)
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  /**
   * EE >: E는 E의 상위 타입을 허용 하여, 더 일반 적인 오류 타입도 가질수 있도록 한다.
   * 함수 f의 결과가 또다른 Either 일 경우에 사용 된다.
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }


  /**
   * Right일 경우 현재 값을 반환하고 Left일 경우 대체값을 반환한다.
   * EE >: E, B >: A 는 각각 오류와 값을 더 일반적인 타입으로 승격시킨다.
   */
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  /**
   * 2개의 Either를 결합하여 새로운 Either를 만든다. 두값이 모두 Right인 경우에만 함수 f를 적용하고
   * 그렇지 않은경우 첫번째 Left를 반환한다.
   */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b1)) => Right(f(a, b1))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object CustomEither {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("빈 리스트 입니다!")
    else
      Right(xs.sum / xs.length)


  def safe(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }


  // 타입 안정성을 위한 커스텀 에러 형식
  case class AppError(message: String, code: String = "UNKNOWN_ERROR")

  /**
   * AppError 사용 예시
   *
   * def process(input: String): Either[AppError, Int] =
   * if (input.isEmpty) Left(AppError("입력값이 비어있습니다", "EMPTY_INPUT"))
   * else Right(input.length)
   *
   * // 사용
   * process("") match {
   * case Left(error) => println(s"오류 발생: ${error.code} - ${error.message}")
   * case Right(value) => println(s"결과: $value")
   * }
   */

  // 유저 등록 시뮬레이션을 위한 케이스 클래스
  case class User(name: String, age: Int, email: String)

  def validateName(name: String): Either[AppError, String] =
    if (name.matches("[A-Za-z]+")) Right(name)
    else Left(AppError("이름은 알파벳만 포함해야 합니다.", "INVALID_NAME"))

  def validateAge(age: String): Either[AppError, Int] =
    try {
      val a = age.toInt
      if (a >= 18) Right(a)
      else Left(AppError("사용자는 18세 이상이어야 합니다.", "UNDERAGE"))
    } catch {
      case _: NumberFormatException => Left(AppError("나이는 숫자여야 합니다.", "INVALID_AGE"))
    }

  def validateEmail(email: String): Either[AppError, String] =
    if (email.matches(".+@.+\\..+")) Right(email)
    else Left(AppError("유효하지 않은 이메일 형식입니다.", "INVALID_EMAIL"))

// 연습문제 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil)) { (a, acc) =>
      f(a).flatMap(b => acc.map(bs => b :: bs))
    }

  def main(args: Array[String]): Unit = {
    // mean 함수 테스트
    // 주어진 시퀸스의 평균을 계산 하고 비어 있을 경우 오류를 반환 한다.
    val seq1 = IndexedSeq(1.0, 2.0, 3.0, 4.0, 5.0)
    val seq2 = IndexedSeq.empty[Double]

    println(s"Mean of seq1: ${mean(seq1)}") // Right(3.0)
    println(s"Mean of seq2: ${mean(seq2)}") // Left("빈 리스트 입니다!")

    // safe 함수 테스트
    // 두 숫자를 안전하게 나누며, 예외가 발생할 경우 Left로 처리
    println(s"Safe division 10 / 2: ${safe(10, 2)}") // Right(5)
    println(s"Safe division 10 / 0: ${safe(10, 0)}") // Left(java.lang.ArithmeticException: / by zero)

    // Try 함수 테스트
    // 주어진 표현식을 평가하고 예외가 발생하면 이를 Left로 처리
    println(s"Try to parse '123': ${Try("123".toInt)}") // Right(123)
    println(s"Try to parse 'abc': ${Try("abc".toInt)}") // Left(java.lang.NumberFormatException: For input string: "abc")



    // 사용자 등록 과정 시뮬레이션

    // map 사용 예시
    val nameValidation = validateName("John").map(_.toUpperCase)
    println(s"Name validation: $nameValidation") // Right(JOHN)

    // flatMap 사용 예시
    val ageValidation = validateAge("25").flatMap { age =>
      if (age > 60) Left(AppError("60세 이하여야 합니다.", "TOO_OLD"))
      else Right(age)
    }
    println(s"연령 검증 결과: $ageValidation") // Right(25)

    // orElse 사용 예시
    val emailValidation = validateEmail("invalid_email")
      .orElse(Right("default@example.com"))
    println(s"이메일 검증 결과: $emailValidation") // Right(default@example.com)

    // map2 사용 예시
    val userRegistration = validateName("Alice").map2(validateAge("30")) { (name, age) =>
      User(name, age, "alice@example.com")
    }
    println(s"유저 등록 결과: $userRegistration") // Right(User(Alice,30,alice@example.com))

    // 복합 예시
    def registerUser(name: String, age: String, email: String): Either[AppError, User] =
      validateName(name).flatMap { validName =>
        validateAge(age).flatMap { validAge =>
          validateEmail(email).map { validEmail =>
            User(validName, validAge, validEmail)
          }
        }
      }

    val successfulRegistration = registerUser("Jane", "28", "jane@example.com")
    println(s"등록 성공: $successfulRegistration")
    // 결과
    // Right(User(Jane,28,jane@example.com))

    val failedRegistration = registerUser("John123", "17", "john@example")
    println(s"등록 실패: $failedRegistration")
    // 결과
    // Left(AppError(이름은 알파벳만 포함해야 합니다.,INVALID_NAME))

    // 오류 처리 예시
    failedRegistration match {
      case Right(user) => println(s"등록 성공: $user")
      case Left(AppError(message, code)) => println(s"등록 실패 [$code]: $message")
    }


    //연습문제 4.7 사용 예
    // 활용 예시
    def parseInt(s: String): Either[String, Int] =
      try Right(s.toInt)
      catch { case _: NumberFormatException => Left(s"$s 은 정수형 타입이 아닙니다.") }

    val stringList = List("1", "2", "3", "4", "5")
    val mixedList = List("1", "2", "three", "4", "5")

    // sequence 사용 예시
    val parsedList = sequence(stringList.map(parseInt))
    println(s"Parsed list: $parsedList")
    // 출력: Parsed list: Right(List(1, 2, 3, 4, 5))

    val parsedMixedList = sequence(mixedList.map(parseInt))
    println(s"Parsed mixed list: $parsedMixedList")
    // 출력: Parsed mixed list: Left(three 은 정수형 타입이 아닙니다.)

    // traverse 사용 예시
    val traversedList = traverse(stringList)(parseInt)
    println(s"Traversed list: $traversedList")
    // 출력: Traversed list: Right(List(1, 2, 3, 4, 5))

    val traversedMixedList = traverse(mixedList)(parseInt)
    println(s"Traversed mixed list: $traversedMixedList")
    // 출력: Traversed mixed list: Left(three 은 정수형 타입이 아닙니다.)
  }
}
