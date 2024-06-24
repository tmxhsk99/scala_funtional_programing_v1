package chap3

object TreeExample {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /**
   * maximum 함수는 Tree[Int]를 입력으로 받아 가장 큰 값을 반환합니다.
   * Leaf의 경우, 그 값을 반환합니다.
   * Branch의 경우, 왼쪽과 오른쪽 서브트리의 maximum 값을 재귀적으로 계산하여 둘 중 큰 값을 반환
   *
   * @param tree
   * @return
   */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Leaf(3),
        Branch(
          Leaf(4),
          Leaf(5)
        )
      )
    )

    println(s"트리 사이즈: ${size(tree)}")
    println(s"트리 중 최대값: ${maximum(tree)}")
    println(s"트리의 뎁스: ${depth(tree)}")

    // 주어진 함수에 곱하기 2 하는 map 함수
    val mappedTree = map(tree)(x => x * 2)
    println(s"곱하기 2 된 트리: $mappedTree")
  }
}
