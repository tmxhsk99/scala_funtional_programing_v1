package chap13

import chap11.Monad
import chap13.BetterIO.TailRec.run

object BetterIO {
  // 기본 IO trait를 sealed로 만들고 세 가지 제어 흐름을 표현하는 case class들을 정의
  sealed trait TailRec[A] { self =>
      def flatMap[B](f: A => TailRec[B]): TailRec[B] =
        FlatMap(this, f)
      def map[B](f: A => B): TailRec[B] =
        flatMap(f andThen(Return(_)))
    }
    // 1. 순수한 값을 반환
    case class Return[A](a: A) extends TailRec[A]
    // 2. 실제 효과를 실행하는 작업
    case class Suspend[A](resume: () => A) extends TailRec[A]
    // 3. 두 IO를 연결하는 작업
    case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    object TailRec extends Monad[TailRec]{
      def unit[A](a: => A): TailRec[A]= Return(a)
      def flatMap[A,B](a: TailRec[A])(f: A => TailRec[B]):TailRec[B] = a flatMap f
      def suspend[A](a: => TailRec[A]) =
        Suspend(() => ()).flatMap{_ => a}
      // 트램펄린 패턴을 적용한 실행기
      @annotation.tailrec
      def run[A](t: TailRec[A]): A = t match {
        case Return(a) => a
        case Suspend(r) => r()
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
     * Function1[Int, TailRec[Int]]는 Int => TailRec[Int]와 동일한 의미입니다
     * foldLeft의 매개변수 a와 b는 둘 다 Int를 받아서 TailRec[Int]를 반환하는 함수입니다
     *  하나의 함수가 10000번 자기 자신과 합성되는 것을 테스트하는 예제입니다
     */
    val g: Int => TailRec[Int] =
      List.fill(10000)(f).foldLeft(f){ // f 함수를 10000개 만들고
        (a: Function1[Int, TailRec[Int]],
         b: Function1[Int, TailRec[Int]]) => {
          (x: Int) => TailRec.suspend(a(x).flatMap(b))
        }
      }

    def main(args: Array[String]): Unit = {
      val gFortyTwo = g(42)
      println("g(42) = " + gFortyTwo)
      println("run(g(42)) = " + run(gFortyTwo))
    }
  }