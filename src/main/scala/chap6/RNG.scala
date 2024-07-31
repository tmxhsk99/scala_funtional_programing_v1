package chap6

trait RNG {
  def nextInt: (Int, RNG)

  /**
   * RNG.nextInt를 이용해서 0 이상. Int.MaxValue 이하의 난수 정수를 생성하는 함수를 작성하라.
   * nextInt가 Int.MinValue를 돌려주는 구석진 경우 (음이 아닌 대응수가 없다)도 확실히 처리해야한다.
   * Int.MaxValue : 2,147,483,647
   * Int.MinValue : -2,147,483,648
   * 대응수가 없다는 말은 MinValue를 +로 바꾸면 Int의 최대 범위를 벗어나서 대응수가 없다는 뜻
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt // (생성한 랜덤값, 새 상태의 RNG)
    if (i < 0) {
      if (i == Int.MinValue) { // Int.MinValue의 특별한 경우 처리
        (0, r)
      }
      else{
        (-(i + 1), r) // 다른 음수처리
      }
    } else (i, r) // 양수이면 그대로 반환
  }
}
