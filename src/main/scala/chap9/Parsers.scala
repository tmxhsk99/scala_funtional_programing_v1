package chap9

import answer.{Gen, Prop}
import answer.Prop.forAll

trait Parsers[ParseError, Parser[+_]] { self => // 이에 의해 이 Parsers 인스턴스를 지칭하는 self가 도입된다. self는 나중에 ParserOps에 쓰인다.
  /**
   * 파서의 실행 함수
   */
  def run[A](p: Parser[A])(input:String): Either[ParseError,A]

  /**
   * 하나의 문자를 인식 하는 파서
   */
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  /**
   * 입력을 소비하지 않고, 주어지값 a를 성공적으로 반환하는 함수
   * (일반상수값을 승격[lift] 하는 함수)
   */
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  /**
   * 두 파서중 하나만 성공해도 성공하는 파서
   * ex)
   * run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
   * run(or(string("abra"),string("cadabra")))({"cadabra") == Right("cadabra")
   */
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
   * 문자열을 자동으로 Parser[String]으로 변환합니다.
   */
  implicit def string(s: String): Parser[String]

  /**
   * Parser를 ParserOps로 자동 변환하여 추가 메서드를 사용할 수 있게 합니다.
   */
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  /**
   * 파서가 되풀이 되는 경우를 처리 하는 함수
   * run(listOfN(3,"ab"|"cad"))("ababcad") == Right(List("ab","ab","cad"))
   */
  def listOfN[A](n:Int, p: Parser[A]): Parser[List[A]] =
    if(n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n-1, p))(_ :: _)

  /**
   * Parser[String]으로 변환 가능한 모든 타입을 ParserOps[String]으로 변환합니다.
   */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))


  def many[A](p:Parser[A]): Parser[List[A]] =
    map2(p,many(p))(_ :: _) or succeed(List())

  def map[A,B] (a: Parser[A])(f: A => B ): Parser[B]

  /**
   * p 를 한 번 적용한 후, many(p)를 적용한다.
   * map
   */
  def manay1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a,list) => a::list)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  /**
   * product를 사용해 두 p1, p2를 순차적으로 적용한다.
   * product의 결과 (A,B) 타입의 튜플이다.
   * 이 튜플에 map을 적용하여 함수 f를 호출하고, 결과적으로 C 타입 값을 얻는다.
   */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2) map {f.tupled}

  /**
   * 이 클래스는 Parser에 추가 메서드를 제공합니다.
   *
   * | 메서드: 중위 연산자 구문으로 or 연산을 수행합니다.
   * or 메서드: |와 동일한 기능을 수행하지만 일반 메서드 호출 구문을 사용합니다.
   *
   */
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2:Parser[B]): Parser[B] = self.or(p,p2)// trait 에 대한 or 메서드 참조의 모호함을 self를 이용해서 명시적으로 해소한다
    def or[B>:A](p2:Parser[B]): Parser[B] = self.or(p,p2)// trait 에 대한 or 메서드 참조의 모호함을 self를 이용해서 명시적으로 해소한다
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)
    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
  }


  /**
   * map의 법칙을 검증하기위한 Object
   */
  object Laws {
    /**
     * 두 파서의 동작이 동일한지 검증하는 헬퍼 함수
     */
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    /**
     * map의 법칙이 맞는지 확인한다.
     */
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
