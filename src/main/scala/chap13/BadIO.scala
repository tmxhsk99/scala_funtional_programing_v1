import org.checkerframework.checker.units.qual.A

import scala.annotation.tailrec

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    def run = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    def run = f(self.run).run
  }
}

object IO {
  def apply(value: Any): IO[A] = ???

  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }

  // println을 감싸는 함수
  def printLn(msg: String): IO[Unit] =
    unit(println(msg))
}

// 개선된 온도 변환 프로그램
object ImprovedIOTest {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def printTemp(f: Double): IO[Unit] =
    IO.printLn(s"${f}F = ${fahrenheitToCelsius(f)}C")


  def printMany(count: Int, temp: Double): IO[Unit] = {
    if (count <= 0) IO.unit(())
    else for {
      _ <- printTemp(temp)
      _ <- printMany(count - 1, temp)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    printMany(10000, 98.6).run
  }
}
