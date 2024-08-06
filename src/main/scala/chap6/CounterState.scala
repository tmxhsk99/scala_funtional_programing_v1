package chap6

import State._


object counterState{
  // 카운터 상태를 위한 타입 별칭
  type CounterState[A] = State[Int, A]

  def increment: CounterState[Int] = for {
    n <- get[Int] // 현재 카운터 값을 가져옴
    _ <- set(n + 1) // 카운터 값을 1증가시킨다.
  } yield n + 1 // 증가된 값을 반환

  def decrement: CounterState[Int] = for {
    n <- get[Int] // 현재 카운터를 가져옴
    _ <- set(n - 1) //카운터값을 1감소시킴
  } yield n - 1 // 감소된 값을 반환

  // 카운터 사용 예시
  /**
   * 어떤 특수한 상태를 만들어야하는 비즈니스로직
   */

  val program: CounterState[Int] = for {
    _ <- increment
    _ <- increment
    c1 <- get[Int]
    _ <- decrement
    c2 <- get[Int]
  } yield c2

  def main(args: Array[String]): Unit = {
    val (result, finalState) = program.run(0)
    println(s"Result: $result, Final state: $finalState")
  }
}




