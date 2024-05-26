package chap1.impure

/**
 * 목록 1.1 부수 효과가 있는 프로그램
 */
class Cafe { // Java에서 처럼 클래스 키워드 클래스를 도입한다 중괄호 사이가 본문

  def buyCoffee(cc: CreditCard): Coffee = { // 클래스의 메서드는 def 키워드를 도입한다.

    val cup = new Coffee() // 세미콜론이 없어도 된다. 한블록의 문장들은 새줄로 구분된다.

    cc.charge(cup.price) // 부수효과 부분 실제로 신용카드 지불 로직

    cup // return이 없어도 된다. 블록 마지막 문장이 자동으로 반환

  }

}
