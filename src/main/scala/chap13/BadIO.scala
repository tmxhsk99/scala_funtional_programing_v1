package chap13

import chap13.BadIO._


object BadIO {
  trait IO { self =>
    def run: Unit
    // ++ 연산자는 두 IO를 순차적으로 실행
    def ++(io: IO): IO = new IO {
      def run = {self.run; io.run} // 여기서 직접 run을 호출
    }
  }
  object IO {
    // 아무것도 하지 않는 빈 IO
    def empty: IO = new IO { def run = {}}
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f -32) * 5.0/9.0


}

object BadIoTest{
  def main(args: Array[String]): Unit = {
    // 온도를 변환하고 출력하는 IO 생성
    def printTemp(f: Double): IO = new IO {
      def run = println(s"${f}F = ${fahrenheitToCelsius(f)}C")
    }

    // 여러 온도를 연속해서 출력하려고 할 때
    def printMany(count: Int, temp: Double): IO = {
      if (count <= 0) IO.empty
      else printTemp(temp) ++ printMany(count - 1, temp)
    }

    // 이렇게 실행하면 큰 숫자에서 스택오버플로우 발생!
    printMany(10000, 98.6).run
  }
}