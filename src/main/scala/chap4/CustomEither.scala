package chap4

object CustomEither {
  type ValidationResult[A] = Either[List[String], A]

  case class User(name: String, age: Int, email: String)

  // 헬퍼 함수들
  def validateName(name: String): ValidationResult[String] =
    if (name.matches("[A-Za-z]+")) Right(name)
    else Left(List(s"Invalid name: $name. 이름은 알파벳으로 이루어져야 합니다."))

  def validateAge(age: String): ValidationResult[Int] =
    try {
      val a = age.toInt
      if (a >= 18) Right(a)
      else Left(List(s"Invalid age: $age. 18세 이상만 가입 가능."))
    } catch {
      case _: NumberFormatException => Left(List(s"Invalid age: $age. 나이는 숫자여야 합니다."))
    }

  def validateEmail(email: String): ValidationResult[String] =
    if (email.matches(".+@.+\\..+")) Right(email)
    else Left(List(s"Invalid email: $email. 이메일이 올바른 형식이 아님."))

  // 여러 ValidationResult를 결합하는 함수
  def map2[A, B, C](va: ValidationResult[A], vb: ValidationResult[B])(f: (A, B) => C): ValidationResult[C] =
    (va, vb) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }

  // 3개의 ValidationResult를 결합하는 함수
  def map3[A, B, C, D](va: ValidationResult[A], vb: ValidationResult[B], vc: ValidationResult[C])(f: (A, B, C) => D): ValidationResult[D] =
    (va, vb, vc) match {
      case (Right(a), Right(b), Right(c)) => Right(f(a, b, c))
      case (Left(e1), Left(e2), Left(e3)) => Left(e1 ++ e2 ++ e3)
      case (Left(e1), Left(e2), _) => Left(e1 ++ e2)
      case (Left(e1), _, Left(e3)) => Left(e1 ++ e3)
      case (_, Left(e2), Left(e3)) => Left(e2 ++ e3)
      case (Left(e), _, _) => Left(e)
      case (_, Left(e), _) => Left(e)
      case (_, _, Left(e)) => Left(e)
    }

  implicit class ValidationResultOps[A](val self: ValidationResult[A]) extends AnyVal {
    def orElse(that: => ValidationResult[A]): ValidationResult[A] = self match {
      case Right(_) => self
      case Left(e1) => that match {
        case Right(_) => that
        case Left(e2) => Left(e1 ++ e2)
      }
    }
  }

  // traverse 구현
  def traverse[A, B](as: List[A])(f: A => ValidationResult[B]): ValidationResult[List[B]] =
    as.foldRight[ValidationResult[List[B]]](Right(Nil)) { (a, acc) =>
      map2(f(a), acc)(_ :: _)
    }

  // sequence 구현
  def sequence[A](as: List[ValidationResult[A]]): ValidationResult[List[A]] =
    traverse(as)(identity)


  def registerUser(name: String, age: String, email: String): ValidationResult[User] =
    map3(validateName(name), validateAge(age), validateEmail(email))(User)

  def main(args: Array[String]): Unit = {
    // 성공 케이스
    val successResult = registerUser("John", "25", "john@example.com")
    println(s"Success case: $successResult")

    // 실패 케이스 (모든 필드가 잘못된 경우)
    val failureResult = registerUser("John123", "17", "invalid-email")
    println(s"Failure case: $failureResult")

    // 결과 처리
    failureResult match {
      case Right(user) => println(s"User registered successfully: $user")
      case Left(errors) =>
        println("Registration failed. Errors:")
        errors.foreach(println)
    }

    // orElse 테스트
    val result1 = validateAge("17").orElse(validateAge("18"))
    println(s"orElse test 1: $result1")

    val result2 = validateAge("15").orElse(validateAge("16"))
    println(s"orElse test 2: $result2")

    // traverse 테스트
    val ages = List("20", "25", "30", "invalid", "35")
    val traverseResult = traverse(ages)(validateAge)
    println(s"traverse test: $traverseResult")

    // sequence 테스트
    val validations = List(validateName("John"), validateAge("25"), validateEmail("john@example.com"))
    val sequenceResult = sequence(validations)
    println(s"sequence test: $sequenceResult")

  }
}