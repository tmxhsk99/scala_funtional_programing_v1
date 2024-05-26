package chap1.addPayments

import chap1.impure.CreditCard

class Payments {
  def charge(cc: CreditCard, price: Double): Void = {
    println("Payments 객체 지불 함수")
    Void
  }
}
