package chap6

trait RNG {
  def nextInt: (Int, RNG)
}
