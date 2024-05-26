package chap1.removeSideEffect

import chap1.impure.CreditCard

/**
 * case 클래스는 클래스이름 (Charge) 다음에 인수 목록이 하나의 주생성자로 구성된다.
 * 이 목록의 매개변수들은 클래스이 공용(public). 수정불가(unmodifiable: 불변(immutable) 필드들이 된다.
 * 이 필드들에는 other.cc 같은 통상 적인 객체 지향적 마침표 표기법으로 접근할 수 있다.
 * @param cc
 * @param amount
 */
case class Charge(cc: CreditCard, amount: Double) {
  def conbine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("다른 카드로는 함께 지불할 수 없습니다")

}
