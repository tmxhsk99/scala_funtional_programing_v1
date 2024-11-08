package chap11

import answer.Gen

case class Order(item: Item, quantity: Int)

case class Item(name: String, price: Double)


object Order {

  val genItem: Gen[Item] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform.map(_ * 10)
  } yield Item(name, price)

  val genOrder: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100) // 1 에서 100 사이의 Int 난수
  } yield Order(item, quantity)

}

