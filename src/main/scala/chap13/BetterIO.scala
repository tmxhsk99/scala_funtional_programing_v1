package chap13

import chap11.Monad
import chap13.BetterIO.TailRec.run

object BetterIO {
  // 기본 IO trait를 sealed로 만들고 세 가지 제어 흐름을 표현하는 case class들을 정의

  /**
   * TailRec라는 trait(인터페이스)를 만듬.
   * sealed 이 trait를 상속받는 클래스들이 모두 이 파일안에 있어야한다는 의미
   */
  sealed trait TailRec[A] { self =>

    /**
     * A타입 값을 받아서 TailRec[B]를 반환하는 함수 f를 받아서
     * 현재 값(this)와 함수 f를 FlatMap으로 감싸서 반환한다
     */
      def flatMap[B](f: A => TailRec[B]): TailRec[B] =
        FlatMap(this, f)

    /**
     * A 타입의 값을 B 타입으로 변환하는 함수 f를 받아서
     * 그 결과를 Return으로 감싸고 flatMap을 사용해 연결합니다.
     */
      def map[B](f: A => B): TailRec[B] =
        flatMap(f andThen(Return(_)))
    }

    // 1. 순수한 값을 반환 단순히 값을 감싸는 케이스 클래스 계산이 끝났을 때 최종 결과를 담는 용도이다.
    case class Return[A](a: A) extends TailRec[A]
    // 2. 실제 계산을 나중으로 미루는 케이스 클래스입니다 resume은 실제 계산을 실행하는 함수이다.
    case class Suspend[A](resume: () => A) extends TailRec[A]
    // 3. 두 계산을 연결하는 케이스 클래스입니다, sub는 첫 번째 계산, k는 그 결과를 받아서 다음 계산을 만드는 함수이다.
    case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    // TailRec의 동반 객체를 만들고 Monad를 상속받습니다.
    object TailRec extends Monad[TailRec]{
      // 일반 값을 TailRec으로 감싸는 함수입니다.
      def unit[A](a: => A): TailRec[A]= Return(a)
      // 모나드의 flatMap 구현입니다.
      def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]):TailRec[B] = a flatMap f
      // 계산을 잠시 중단했다가 다시 시작할 수 있게 해주는 함수입니다.
      def suspend[A](a: => TailRec[A]) =
        Suspend(() => ()).flatMap{_ => a}
      // TailRec을 실제로 실행하는 함수입니다.
      // @annotation.tailrec은 이 함수가 꼬리 재귀 최적화가 가능하다는 것을 보장합니다.
      @annotation.tailrec
      def run[A](t: TailRec[A]): A = t match {
        // Return의 경우 단순히 값을 반환합니다.
        case Return(a) => a
        // Suspend의 경우 미뤄둔 계산을 실행합니다.
        case Suspend(r) => r()
        // FlatMap의 경우 내부 패턴 매칭을 통해 다시 처리합니다
        case FlatMap(x, f) => x match {
          case Return(a) => run(f(a))
          case Suspend(r) => run(f(r()))
          case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
        }
      }

    }

  }
  object BetterIOTest {
    import BetterIO._

    // 기본 함수: 입력 받은 정수를 그대로 TailRec으로 감싸서 반환
    val f: Int => TailRec[Int] = (i: Int) => Return(i)

    /**
     * 정수 x를 입력받고
     * 먼저 a(x)를 실행하고
     * 그 결과에 b를 flatMap으로 연결합니다
     * 전체를 TailRec.suspend로 감싸서 스택 안전성을 보장합니다
     * 전체적으로 이 코드는 f 함수를 10000번 중첩해서 실행하는 함수를 만듭니다. 각각의 함수 호출은 TailRec으로 감싸져 있어서, 깊은 중첩에도 불구하고 스택 오버플로우가 발생하지 않도록 설계되어 있습니다.
     */
    val g: Int => TailRec[Int] =
      List.fill(10000)(f).foldLeft(f){ // f 함수를 10000개 만들고
        (a: Int => TailRec[Int],
         b: Int => TailRec[Int]) => {
          (x: Int) => TailRec.suspend(a(x).flatMap(b))
        }
      }

    def main(args: Array[String]): Unit = {
      val gFortyTwo = g(42)
      println("g(42) = " + gFortyTwo)
      println("run(g(42)) = " + TailRec.run(gFortyTwo))
    }
  }
