package chap1.removeSideEffect

import chap1.impure.{Coffee, CreditCard}

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) ={
    val cup = new Coffee()
    (cup, Charge(cc,cup.price))
  }

  /**
   * 목록 1.3 여러 잔의 커피를 구매하는 buyCoffes
   *
   *
   */
  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee],Charge) = { // List [Coffee]는 Coffee 값들의 불변이 단일 연결 목록이다. 이 자료 형식에 대해서는 3장에서 논의 한다.
    /**
     * LIst.fill(n)(x) x의 복사본 n개로 이루어진 List를 생성한다.
     */
    val purchase: List[(Coffee,Charge)] = List.fill(n)(buyCoffee(cc))

    // unzip은 쌍들의 목록을 목록 들의 쌍으로 분리 한다. 하나의 쌍을 두개의 값 (coffees 와 charges)으로 해체하는 작업을 한줄의 코드로 표현했다.
    val (coffees, charges) = purchase.unzip
    (coffees, charges.reduce((c1,c2) => c1.conbine(c2))) //chares.reduce는 한번에 청구건 두개를 combine을 이용해서 하나로 결합하는 과정을 반복 함 으로서  청구 건들의 목록 전체를 하나의 청구 건으로 환원(reduce)한다.
  }
}
