package chap7

import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit}
import scala.concurrent.TimeoutException


object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
   * unit은 UnitFuture를 돌려주는 함수로 표현된다.
   * UnitFuture는 Futrue의 간단한 구현으로, 그냥 상수 값을 감싸기만 할 뿐
   * ExecutorService는 전혀 사용하지 않는다. UnitFuture는 항상 완료 가능하며, 취소는 불가능하다.
   * UnitFutrue의 get 메서드는 이전에 주어진 상수값을 돌려주기만 한다.
   * */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /**
   * 이 API 에서 병렬성 제어는 오직 fork 함수만 담당한다는 설계상의 선택에 따라,
   * map2는 f 호출을 개별 논리적 스레드에서 평가하지 않는다.
   * f를 개별 스레드에서 평가 하고 싶다면 fork(map2(a,b)(f))를 사용하면 된다.
   */
  def _map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      /**
       * map2의 이 구현은 만료시간을 지키지 않는다.
       * 이 구현은 그냥 ExecutorService를 두 Par 값에 전달하고, Future의 af와 bf의 결과들을 기다리고,
       * 그것들에 f를 적용하고, 적용결과를 UnitFuture로 감쌀 뿐이다.
       * 만료 시간을 지키기위해서는 af의 평가에 걸린 시간을 측정하고 bf의 평가에 걸린시간에서
       * 그 시간을 빼는 식으로 새로운 Future 구현이 필요할 것이다.
       */
      UnitFuture(f(af.get, bf.get))
    }

  /**
   * 7.3 Future의 만료 시간 계약을 존중하도록 map2 구현 개선
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {

      private val af = a(es)
      private val bf = b(es)

      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean =
        af.isCancelled || bf.isCancelled

      override def isDone: Boolean =
        af.isDone && bf.isDone

      override def get(): C =
        get(Long.MaxValue, TimeUnit.NANOSECONDS)

      def nanoToSeconds(nanos: Long): Double = nanos / 1e9

      override def get(timeout: Long, unit: TimeUnit): C = {
        val timeoutNanos = unit.toNanos(timeout)
        val start = System.nanoTime()
        val ar = af.get(timeoutNanos, TimeUnit.NANOSECONDS)

        val elapsed = System.nanoTime - start
        val timeLeft = timeoutNanos - elapsed
        println(f"start: ${nanoToSeconds(start)}%.3f 초")
        println(f"elapsed: ${nanoToSeconds(elapsed)}%.3f 초")
        println(f"timeLeft: ${nanoToSeconds(timeLeft)}%.3f 초")
        if (timeLeft <= 0) throw new TimeoutException()
        val br = bf.get(timeLeft, TimeUnit.NANOSECONDS)
        f(ar, br)
      }
    }

  /**
   * 이것이 fork의 가장 간단하고 자연스러운 구현이나,
   * 몇가지 문제점이 있다
   * 예를 들어 외곽의 Callable 은 '안쪽'과제가 완료될 때까지 차단된다.
   * 이러한 차단이 스레드 풀의 한 스레드(또는 ExecutorService의 내부에 쓰이는 어떤 자원)를 점유하며,
   * 이는 잠재적 병렬성의 일부가 소실될 수 있음을 의미한다.
   * 본질적으로 이 구현은 한 스레드로 충분한 작업을 두 개의 스레드로 수행한다.
   * 이는 이 구현의 좀 더 심각한 문제점(이번 장에서 나중에 논의함)의 증상이다.
   */
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 연습 문제 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def _sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2( parList, unit( () ) )( (a, _) => a.sorted )

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map(asyncF(a =>
      if (f(a)) List(a) else List()
    ))
    map(sequence(pars))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val defaultTimeout = 10;
      val index = run(es)(n).get(defaultTimeout, TimeUnit.SECONDS) // Add appropriate timeout
      run(es)(choices(index))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t,f))

  /**
   * 실행을 위한 run 함수
   */
  def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

  def main(args: Array[String]): Unit = {
    // 병렬 계산을 위한 ExecutorService
    val es: ExecutorService = Executors.newFixedThreadPool(4)

    // 비용이 높은 계산 1
    def expensiveCompute4Sec(n: Int): Int = {
      Thread.sleep(4000)
      println(s"4초 계산 완료: ${n * n * n}")
      n * n * n
    }

    // 비용이 높은 계산 2
    def expensiveCompute5Sec(n: Int): Int = {
      println(s"5초 계산 완료: ${n * n}")
      Thread.sleep(5000)
      n * n
    }

    try {
      // 7.3 연습 문제 사용 예

      // 두병렬 계산 결과를 합친다 라는 함수
      val combined: Par[Int] = map2(lazyUnit(expensiveCompute4Sec(2)), lazyUnit(expensiveCompute5Sec(3)))(_ + _)

      println("계산 시작...")
      val startTime = System.currentTimeMillis()

      // 제한 시간
      val limitedTime = 6

      try {
        // Future를 얻고 나중에 결과를 가져오는 방식
        val result: Future[Int] = run(es)(combined)
        val finalResult = result.get(limitedTime, TimeUnit.SECONDS)

        val endTime = System.currentTimeMillis()
        println(s"실행 시간: ${(endTime - startTime) / 1000.0} 초")
        println(s"결과: $finalResult")
      } catch {
        case _: TimeoutException =>
          println(s"계산이 ${limitedTime}초 제한 시간을 초과했습니다.")
        case e: Exception =>
          println(s"오류 발생: ${e.getMessage}")
      }



      // sequence 사용 예시
      println("\n=== sequence 예제 ===")
      val par1 = lazyUnit(expensiveCompute4Sec(2))
      val par2 = lazyUnit(expensiveCompute5Sec(3))
      val par3 = lazyUnit(expensiveCompute4Sec(4))

      val sequenced: Par[List[Int]] = sequence(List(par1, par2, par3))

      val sequenceResult: Future[List[Int]] = run(es)(sequenced)
      println("Sequence 결과: " + sequenceResult.get(10, TimeUnit.SECONDS))

      // parMap 사용 예시
      println("\n=== parMap 예제 ===")
      val numbers = List(1, 2, 3, 4, 5)
      val squarePar: Par[List[Int]] = parMap(numbers)(n => {
        Thread.sleep(1000) // 각 계산에 1초의 지연을 추가
        n * n
      })

      val squareResult: Future[List[Int]] = run(es)(squarePar)
      println("ParMap 결과: " + squareResult.get(6, TimeUnit.SECONDS))


      // choiceN과 choice 예제
      println("\n=== choiceN과 choice 예제 ===")

      // choiceN 사용 예시
      val choiceNExample = {
        // 0, 1, 2 중 하나를 무작위로 선택하는 Par[Int]
        val n: Par[Int] = lazyUnit {
          val random = new scala.util.Random()
          random.nextInt(3)
        }

        // 선택 가능한 세 가지 계산
        val choices: List[Par[String]] = List(
          lazyUnit {
            Thread.sleep(1000)
            "첫 번째 선택"
          },
          lazyUnit {
            Thread.sleep(1000)
            "두 번째 선택"
          },
          lazyUnit {
            Thread.sleep(1000)
            "세 번째 선택"
          }
        )

        choiceN(n)(choices)
      }

      println("choiceN 결과: " + run(es)(choiceNExample).get(5, TimeUnit.SECONDS))

      // choice 사용 예시
      val choiceExample = {
        // true 또는 false를 무작위로 선택하는 Par[Boolean]
        val cond: Par[Boolean] = lazyUnit {
          val random = new scala.util.Random()
          random.nextBoolean()
        }

        // 조건이 참일 때의 계산
        val trueChoice: Par[String] = lazyUnit {
          Thread.sleep(1000)
          "조건이 참입니다"
        }

        // 조건이 거짓일 때의 계산
        val falseChoice: Par[String] = lazyUnit {
          Thread.sleep(1000)
          "조건이 거짓입니다"
        }

        choice(cond)(trueChoice, falseChoice)
      }

      println("choice 결과: " + run(es)(choiceExample).get(5, TimeUnit.SECONDS))

    } finally {
      es.shutdown()
    }


  }

}