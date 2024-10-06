package chap9

import answer.{Gen, Prop}
import answer.Prop.forAll

import java.util.regex.Pattern
import scala.util.matching.Regex

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

  implicit def regex(r: Regex): Parser[String]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // 이 파서는 먼저 하나의 숫자를 파싱한 다음, 그 숫자만큼의 'a' 문자를 파싱합니다.
  // 예: "3aaa"는 성공적으로 파싱되지만, "3aa"나 "3aaaa"는 실패합니다.

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

  def map[A,B] (a: Parser[A])(f: A => B ): Parser[B] =
    flatMap(a)(f andThen succeed)

  /**
   * p 를 한 번 적용한 후, many(p)를 적용한다.
   * map
   */
  def manay1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a,list) => a::list)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b =>(a,b)))

  /**
   * product를 사용해 두 p1, p2를 순차적으로 적용한다.
   * product의 결과 (A,B) 타입의 튜플이다.
   * 이 튜플에 map을 적용하여 함수 f를 호출하고, 결과적으로 C 타입 값을 얻는다.
   */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p; b <- p2 } yield f(a,b)

  // JSON 파서 관련 구현

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /**
   * 첫번째 파서의 겨로가를 무시하고 두번째 파서의 결과만 반환한다.
   * 예: skipL(string("("), digit) 는 "(3" 입력에서 3을 반환한다.
   */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /**
   * 첫 번째 파서의 결과만 반환하고 두 번째 파서의 결과를 무시한다.
   * 예: skipR(digit, string(")")) sms "3)" 입력에서 3을 반환한다.
   */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  /**
   * 파서를 옵션으로 만든다. 파싱에 실패하면 None을 반환한다.
   * 예: opt(digit)는 "a" 입력에서 None을 ,"3"입력에서 Some(3)을 반환한다.
   */
  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /**
   * 0 이상의 공백 문자를 파싱한다.
   * JSON에서 요소사이의 공백을 무시할 때 사용한다.
   */
  def whitespace: Parser[String] = "\\s*".r

  /**
   * 1개 이상의 숫자를 파싱한다.
   * JSON 숫자의 정수 부분을 파싱할 때 사용한다.
   */
  def digits: Parser[String] = "\\d+".r

  /**
   * 주어진 문자열을 만날 때까지 모든 문자를 파싱한다.
   * 예: thru("}") 는 JSON 객체의 끝을 찾는 데 사용할 수 있다.
   */
  def thru(s: String): Parser[String] = (".*?"+ Pattern.quote(s)).r

  /**
   * 따옴표로 둘러싸인 문자열을 파싱한다.
   * JSON 문자열 값을 파싱할 때 사용한다.
   */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /**
   * 이스케이프 문자를 포함한 문자열을 파싱한다.
   * JSON의 복잡한 문자열 값(예: 줄바꿈, 따옴표 포함)을 파싱할 때 사용한다.
   */
  def escapedQuoted: Parser[String] =
    token(quoted label "문자열 값")

  /**
   * 부동소수점 숫자를 문자열로 파싱한다.
   * JSON의 숫자 값을 정확하게 파싱할때 사용한다.
   */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /**
   * 부동소수점 숫자를 Double 타입으로 파싱한다.
   * JSON 요소를 파싱한 후 불필요한 공백을 제거할 때 사용한다.
   */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "더블 값"

  /**
   * 파서를 실행하고 그 뒤의 공백을 무시한다.
   * JSON 요소를 파싱한 후 불필요한 공백을 제거할 때 사용한다.
   */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /**
   * 구분자로 분리된 0개 이상의 요소를 파싱한다.
   * JSON 요소를 파싱할때 사용한다.
   */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /**
   * 구분자로 분리된 1개 이상의 요소를 파싱합니다.
   * 최소한 하나의 요소를 가진 JSON 배열을 파싱할 때 사용합니다.
   */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /**
   * 좌결합 연산자를 파싱합니다.
   * JSON에서는 직접적으로 사용되지 않지만, 복잡한 수식 파싱에 유용할 수 있습니다.
   */
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] =
    map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  /**
   * 시작과 끝 구분자 사이의 내용을 파싱합니다.
   * JSON 객체나 배열을 파싱할 때 사용합니다.
   */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /**
   * 입력의 끝을 확인합니다.
   * JSON 파싱이 완전히 끝났는지 확인할 때 사용합니다.
   */
  def eof: Parser[String] =
    regex("\\z".r).label("예상치 못한 추가 입력")

  /**
   * 전체 JSON을 파싱하고 입력의 끝을 확인합니다.
   * 완전한 JSON 문서를 파싱할 때 사용합니다.
   */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof


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

    // JSON 파서
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
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
