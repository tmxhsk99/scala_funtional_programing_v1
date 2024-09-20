package chap8

import chap6.{RNG, State}

/**
 * Gen[A]: A 타입의 값을 생성하는 생성기를 표현하는 클래스
 */
case class Gen[A](sample: State[RNG,A]){

  /**
   * unit: 주어진 값 a를 항상 생성하는 Gen을 만든다.
   * a: => A는 Lazy evaluation을 의미한다. 값이 실제로 필요할 때까지 평가를 지연시킨다.
   */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  /**
   * choose: start(포함)부터 stopExclusive(제외) 사이의 랜덤한 정수를 생성하는 Gen을 만든다.
   */
  def choose (start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /**
   * ListOfN: 주어진 생성기 g를 n번 사용하여 길이 n의 리스트를 생성하는 Gen을 만든다.
   */
  def listOfN[A](n: Int,g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
   * 한 생성기의 결과를 다른 생성기의 입력으로 사용할수 있게한다.
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  /**
   * 동적 버전 크기 자체를 생성기를 사용하여 두 생성기중 하나를 균등하게 선택한다.
   */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))

  /**
   * 두 생성기중 하나를 균등하게 선택한다.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def boolean: Gen[Boolean] =
    Gen(State(rng => {
      val (n, nextRNG) = RNG.nonNegativeInt(rng)
      (n % 2 == 0, nextRNG)
    }))

  /**
   * 주어진 가중치에 따라 생성기를 선택한다. 이를 위해 0에서 1사이의 난수를 생성하고,
   * 이 값을 가중치와 비교하여 선택한다.
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val total = weight1 + weight2
    Gen(State(RNG.double).flatMap(d =>
      if (d * total < weight1) gen1.sample else gen2.sample
    ))
  }

}
