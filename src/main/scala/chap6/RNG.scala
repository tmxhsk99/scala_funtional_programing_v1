package chap6

import scala.collection.immutable.List

trait RNG {
  def nextInt: (Int, RNG)



}

object RNG {
  case class Simple(seed: Long) extends RNG{
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
   * RNG.nextInt를 이용해서 0 이상. Int.MaxValue 이하의 난수 정수를 생성하는 함수를 작성하라.
   * nextInt가 Int.MinValue를 돌려주는 구석진 경우 (음이 아닌 대응수가 없다)도 확실히 처리해야한다.
   * Int.MaxValue : 2,147,483,647
   * Int.MinValue : -2,147,483,648
   * 대응수가 없다는 말은 MinValue를 +로 바꾸면 Int의 최대 범위를 벗어나서 대응수가 없다는 뜻
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt // (생성한 랜덤값, 새 상태의 RNG)
    if (i < 0) {
      if (i == Int.MinValue) { // Int.MinValue의 특별한 경우 처리
        (0, r)
      }
      else {
        (-(i + 1), r) // 다른 음수처리
      }
    } else (i, r) // 양수이면 그대로 반환
  }

  /**
   * 연습문제 6.2
   * 0이상, 1미만 Double 난수를 발생하는 함수를 작성 하라.
   * 참고: 최대 양의 정수를 얻으려면 Int.MaxValue를,x:Int를 변환하려면
   * x.toDouble을 사용하면 된다.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val d = math.abs(i.toDouble / Int.MinValue.toDouble)
    (d, r)
  }

  /**
   * 연습문제 6.3
   * 각각 난수쌍(Int,Double)하나. 3튜플(Double,Double,Double)
   * 하나를 발생하는 함수들을 작성하라.
   * 앞서 작성한 함수들을 재사용 할 수 있어야한다.
   * def intDouble(rng:RNG): ((Int,Double), RNG)
   * def doubleInt(rng:RNG): ((Double,Int), RNG)
   * def double3(rng:RMG): ((Double,Double,Double), RNG)
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
   * 연습문제 6.4
   * 정수 난수 들의 목록을 생성하는 함수를 작성하자.
   * def ints(count:Int)(rng:RNG): (List[Int], RNG)
   */
  // 꼬리 재귀 (TCR Tail Call Recursive) 를 사용한 버전
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    // 내부 보조 함수
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r) // 카운트 가 0인 경우 현재까지의 난수 리스트와 RNG 상태 반환
      else {
        val (x, r2) = r.nextInt // 새로운 난수와 RNG 상태 생성
        go(count - 1, r2, x :: xs) // 재귀 호출 : 카운트 감서, 새 상태의 RNG, 새 난수를 리스트 앞에 추가
      }

    go(count, rng, List())
  }

  // 일반 재귀를 사용한 버전
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng) // 카운트가 0이면 빈리스트와 현재 RNG 상태반환
    else {
      val (x, r1) = rng.nextInt // 새로운 난수 생성과 새 상태 RNG k생성
      val (xs, r2) = ints(count - 1)(r1) // 재귀호출 : 카운트 감소, 새 RNG 상태로 나머지 리스트 생성
      (x :: xs, r2) // 현재 난수를 리스트 앞에 추가하고, 최종 RNG 상태와 함께 반환
    }

  /**
   * 공통 형태의 별칭
   */
  type Rand[+A] = RNG => (A, RNG)

  /**
   * 순수한 값을 Rand 컨텍스트에 넣을 때 사용됨
   * 매개변수로 받은 값만 고정됨
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
   * Rand[A] 형태의 함수와 A=> B로 타입을 변경하는 함수를 받아
   * Rand[B] 로 만든다.
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * 양수이고 짝수인 RNG => (A, RNG) 형태로 바꿔줌
   */
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  /**
   * 연습문제 6.5
   * 연습문제 6.2의  double을 map을 이용해서 좀 더 우아한 방식으로 구현하라
   */
  val _double: Rand[Double] =
    map(nonNegativeInt)((i: Int) => i / (Int.MaxValue.toDouble + 1))


  /**
   *   다음과 같은 서명에 따라 map2를 구현하라.
   * 이 함수는 두상태 동작 ra 및 rb와 이들의 결과를 조합하는 함수 f를 받고 두 동작을 조합한 새 동작을 돌려준다.
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  /**
   * map2를 작성하면 임의의 RNG 상태 동작들을 조합할 수 있다.
   * 그 둘을 조합해서 A와 B의 쌍을 발생하는 동작을 얻을 수 있다.
   */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /**
   * 더 간결 하게 구현한 6.3
   */
  val randIntDouble: Rand[(Int, Double)] =
    both(nonNegativeInt, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, nonNegativeInt)

  /**
   * - 연습문제 6.7
   * ```
   * 어려운: 두 RNG 상태 전이를 조합할 수 있다면, 그런 상태 전이들의 목록 전체를 조합하는 것도 가능해야 마땅하다.
   * 상태 전이들의 List를 하나의 전이로 조합하는 함수 sequence를 구현하라.
   * 그리고 이 함수를 이용해서 이전에 작성한 ints 함수를 다시 구현하라.
   * ints 함수의 구현에서 x가 n 번 되풀이 되는 목록을 만들일이 있으면 표준 라이브러리 함수 List.fill(n)(x)를 사용해도 좋다
   *
   * def sequence[A] (fs: List[Rand[A]]): Rand[List[A]]
   * ```
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (f, acc) =>
      map2(f, acc)(_ :: _)
    }

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))


  /**
   * flatMap을 구현하고 그것을 이용해서 nonNegativesLessThan을 구현하라
   * def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand
   * f: Rand[A] 는 초기 난수 생성함수
   * g: A => Rand[B] 는 f의 결과를 받아 새로운 Rand[B]를 생성하는 함수
   *
   * f를 받아 실행  시키고 그 결과타입 A를 받아
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)

      // g(a)(r1)은 풀어서 보면 다음과 같은 식으로 볼수 있다.
      // val h: Rand[B] = g(a)  // h는 RNG => (B, RNG) 타입의 함수
      // h(r1)  // 최종적으로 (B, RNG) 튜플 반환
    }

  /**
   * 매개변수 int 값 이하 0이상 난수값을 반환한다.
   */
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }


  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))


  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))


}
