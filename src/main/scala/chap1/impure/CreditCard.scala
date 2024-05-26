package chap1.impure

class CreditCard {

  def charge(price: Int): Void = {

    println( price + " 원 지불 처리")

    return Void
  }

}
