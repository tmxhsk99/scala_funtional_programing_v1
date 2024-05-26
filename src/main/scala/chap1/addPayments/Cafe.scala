package chap1.addPayments

import chap1.impure.{Coffee, CreditCard}

class Cafe {
  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()
    p.charge(cc, cup.price)
    cup
  }

}
