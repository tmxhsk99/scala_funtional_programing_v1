package chap6

object main {
  def main(args: Array[String]): Unit = {
    // 임의의 종잣값으로 42를 선택한다
    val rng = SimpleRNG(42)

    // rng.nextInt가 돌려준 쌍을 분해해서 두 개의 값을 선언하는 구문
    val (n1,rng2) = rng.nextInt

    println(s"n1 : $n1")
    println(s"rng2 : $rng2")

    val (n2, rng3) = rng2.nextInt
    println(s"n2 : $n2")
    println(s"rng3 : $rng3")

  }
}
