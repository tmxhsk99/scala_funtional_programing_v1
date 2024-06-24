import List.{dropWhile, productNoneStrict, productStrict}

object Main {
  def printList[A](list: List[A]): String = list match {
    case Nil => ""
    case Cons(h, Nil) => h.toString
    case Cons(h, t) => s"$h, ${printList(t)}"
  }
  def main(args: Array[String]): Unit = {
    val list1 = List(1, 2, 3, 4, 5, 1)
    val result1 = dropWhile(list1, (x: Int) => x < 3)
    println(printList(result1)) // 출력: 3, 4, 5, 1

    val list2 = List(1, 1, 1, 4, 5, 6)
    val result2 = dropWhile(list2, (x: Int) => x < 4)
    println(printList(result2)) // 출력: 4, 5, 6

    val list3 = List(5, 6, 7)
    val result3 = dropWhile(list3, (x: Int) => x < 4)
    println(printList(result3)) // 출력: 5, 6, 7

    val list4 = List(1, 2, 3)
    val result4 = dropWhile(list4, (x: Int) => x < 4)
    println(printList(result4)) // 출력:


    // 강제인수 실행 (즉시평가)
    val list5 = List(1.0, 2.0, 0.0, 3.0, 4.0)
    val result5 = productStrict(list5)
    println(s"즉시평가 결과: $result5")

    println("===================================")
    val list6 = List(1.0, 2.0, 0.0, 3.0, 4.0)
    // 비강제인수 실행 (지연평가)
    val result6 = productNoneStrict(list6)
    println(s"지연평가 결과: $result6")

  }
}