package chap13

import chap11.Monad
import chap13.AsyncIO.{Async, Suspend, run}
import chap7.Par
import chap7.Par.Par

import java.util.concurrent.Executors

object AsyncIO {

  /*
  *  우리는 스택 안전성 문제는 해결했지만, 아직 두 가지 문제가 남아있습니다:
  *  1. 어떤 종류의 효과(effects)가 발생할 수 있는지 명시적이지 않음
  *  2. 비동기 연산을 표현하는 방법을 찾지 못함
  *
  *  현재 `Suspend` thunk는 인터프리터에 의해 실행될 때 현재 스레드를 블록합니다.
  *  이를 해결하기 위해 `Suspend`의 시그니처를 `Par`를 사용하도록 변경했고,
  *  이 새로운 타입을 `Async`라고 부릅니다.
 */

  sealed trait Async[A] {
    // 비동기 연산을 연결하는 메서드
    def flatMap[B](f: A => Async[B]): Async[B] = {
      FlatMap(this, f)
    }

    // 결과를 변환하는 메서드
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }
  // 1. 단순한 값을 담는 케이스
  case class Return[A](a: A) extends Async[A]
  // 2. Par[A] 타입의 비동기 연산을 담는 케이스
  case class Suspend[A](resume: Par[A]) extends Async[A]
  // 3. 연산들을 연결하는 케이스
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A,B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  // step : FlatMap의 중첩을 정리하는 함수
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    // FlatMap이 중첩된 경우 정리
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    // Return 값에 대한 FlatMap 바로 실행
    case FlatMap(Return(x), f) => step(f(x))
    // 그외의 경우는 그대로 반환
    case _ => async
  }

  // Async를 실제로 실행하는 함수
  def run[A](async: Async[A]): Par[A] = step(async) match {
    // 단순 값을 Par.unit으로 변환
    case Return(a) => Par.unit(a)
    // Supend는 담고 있는 Par를 그대로 반환
    case Suspend(r) => r
    // FlatMap은 연산을 연결해서 실행
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

}

import chap13.AsyncIO._
import chap7.Par
import java.util.concurrent.{Executors, TimeUnit}

object AsyncIOExample extends App {
  val es = Executors.newFixedThreadPool(4)

  def simulateIOWork(id: String, delayMs: Long, result: String): Par[String] =
    Par.delay(delayMs) {
      println(s"$id 작업 완료 (${delayMs}ms 소요)")
      result
    }

  // 1. 비동기 작업정의
  def readUserData(userId: Int): Async[String] =
    Suspend(simulateIOWork("사용자 데이터 읽기", 1000, s"User $userId 데이터"))

  def readSystemStatus(): Async[String] =
    Suspend(simulateIOWork("시스템 상태 읽기", 1000, s"시스템 정상"))

  def processResults(userData: String, systemStatus: String): Async[String] =
    Suspend(simulateIOWork("결과 처리", 1000, s"처리완료 - User: $userData, System: $systemStatus"))

  try {
    println("비동기 작업 시작...")
    val startTime = System.currentTimeMillis()

    // 2. 독립적인 작업들을 먼저 시작
    val userDataFuture = Par.run(es)(run(readUserData(123)))
    val systemStatusFuture = Par.run(es)(run(readSystemStatus()))

    println("두 개의 비동기 작업이 백그라운드에서 실행 중...")

    // 메인 스레드에서 다른 작업 수행
    println("메인 스레드 작업 1 시작")
    Thread.sleep(500)
    println("메인 스레드 작업 2 시작")
    Thread.sleep(500)
    println("메인 스레드 작업 3 시작")

    // 3. 두 결과를 가져와서 처리
    val userData = userDataFuture.get(2, TimeUnit.SECONDS)
    println("비동기처리가 끝난 userData 가져옴")
    val systemStatus = systemStatusFuture.get(2, TimeUnit.SECONDS)
    println("비동기처리가 끝난 systemData 가져옴")

    // 결과를 합쳐서 최종 처리
    val finalResult = Par.run(es)(run(processResults(userData, systemStatus))).get(2, TimeUnit.SECONDS)

    val endTime = System.currentTimeMillis()
    println(s"\n작업 완료!")
    println(s"최종 결과: $finalResult")
    println(s"총 소요시간: ${(endTime - startTime)/1000.0}초")
  } finally {
    es.shutdown()
  }
}