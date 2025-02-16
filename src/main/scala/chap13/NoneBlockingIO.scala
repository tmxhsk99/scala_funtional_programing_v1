import cats.effect.{IO, IOApp}
import cats.free.Free
import cats.~>

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

// 가짜 DB 응답을 위한 case class
case class User(id: Int, name: String)

// Mock Database
object MockDatabase {
  private var users = Map[Int, User]()

  def readUser(id: Int): User =
    users.getOrElse(id, User(id, s"User$id"))

  def writeUser(id: Int, name: String): Unit = {
    users += (id -> User(id, name))
  }

  def readUserAsync(id: Int): Future[User] =
    Future.successful(readUser(id))

  def writeUserAsync(id: Int, name: String): Future[Unit] =
    Future.successful(writeUser(id, name))
}

// 1. 연산들의 ADT 정의
sealed trait DatabaseOp[A]
case class ReadUser(id: Int) extends DatabaseOp[User]
case class WriteUser(id: Int, name: String) extends DatabaseOp[Unit]



object DatabaseExample extends IOApp.Simple {  // IOApp.Simple로 변경

  // 2. 차단식 인터프리터
  val blockingInterpreter = new (DatabaseOp ~> IO) {
    def apply[A](op: DatabaseOp[A]): IO[A] = op match {
      case ReadUser(id) =>
        IO(MockDatabase.readUser(id))
      case WriteUser(id, name) =>
        IO(MockDatabase.writeUser(id, name))
    }
  }

  // 3. 비차단식 인터프리터
  val nonblockingInterpreter = new (DatabaseOp ~> Future) {
    def apply[A](op: DatabaseOp[A]): Future[A] = op match {
      case ReadUser(id) =>
        MockDatabase.readUserAsync(id)
      case WriteUser(id, name) =>
        MockDatabase.writeUserAsync(id, name)
    }
  }

  def run: IO[Unit] = {  // run 메서드 구현
    val program = for {
      user <- Free.liftF[DatabaseOp, User](ReadUser(1))
      _ <- Free.liftF[DatabaseOp, Unit](WriteUser(2, "Alice"))
      user2 <- Free.liftF[DatabaseOp, User](ReadUser(2))
    } yield (user, user2)

    // 차단식 실행
    for {
      _ <- IO.println("Using blocking interpreter:")
      result <- program.foldMap(blockingInterpreter)
      (user1, user2) = result
      _ <- IO.println(s"User1: $user1")
      _ <- IO.println(s"User2: $user2")

      // 비차단식 실행
      _ <- IO.println("\nUsing non-blocking interpreter:")
      futureResult = program.foldMap(nonblockingInterpreter)
      nonBlockingResult <- IO.fromFuture(IO(futureResult))
      (futureUser1, futureUser2) = nonBlockingResult
      _ <- IO.println(s"User1: $futureUser1")
      _ <- IO.println(s"User2: $futureUser2")
    } yield ()
  }
}
