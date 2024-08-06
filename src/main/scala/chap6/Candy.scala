package chap6

import chap6.State.{modify, sequence, get}

// 입력 타입 정의
sealed trait Input
case object Coin extends Input  // 동전 투입
case object Turn extends Input  // 손잡이 돌리기

// 자판기 상태 정의
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  // 자판기 상태 업데이트 함수
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s  // 사탕이 없으면 상태 변화 없음
      case (Coin, Machine(false, _, _)) => s  // 이미 잠금 해제 상태면 변화 없음
      case (Turn, Machine(true, _, _)) => s  // 잠긴 상태에서 돌리기 시도하면 변화 없음
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)  // 동전 투입 시 잠금 해제 및 동전 증가
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)  // 손잡이 돌리기 시 사탕 감소 및 잠금
    }

  // 자판기 시뮬레이션 함수
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // 각 입력에 대해 상태 수정 적용
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    // 최종 상태 가져오기
    s <- get
  } yield (s.coins, s.candies)  // 최종 동전 수와 사탕 수 반환

  def main(args: Array[String]): Unit = {
    // 초기 자판기 상태: 잠김, 5개의 사탕, 10개의 동전
    val initialMachine = Machine(locked = true, candies = 5, coins = 10)

    // 시뮬레이션할 입력 시퀀스
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    // 시뮬레이션 실행
    val ((finalCoins, finalCandies), finalMachine) =
      Candy.simulateMachine(inputs).run(initialMachine)

    println(s"초기 상태: $initialMachine")
    println(s"입력 시퀀스: $inputs")
    println(s"최종 동전 수: $finalCoins")
    println(s"최종 사탕 수: $finalCandies")
    println(s"최종 자판기 상태: $finalMachine")
  }
}