package chap1.impure

class CreditCard {

  def charge(price: Double): Unit = {

    println( price + " 원 지불 처리")
  }

}
