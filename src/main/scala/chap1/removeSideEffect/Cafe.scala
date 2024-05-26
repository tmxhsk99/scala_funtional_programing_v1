package chap1.removeSideEffect

import chap1.impure.{Coffee, CreditCard}

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) ={
    val cup = new Coffee()
    (cup, Charge(cc,cup.price))
  }

}
