package math.algebra

/**
 * @see https://github.com/non/spire
 * @see https://twitter.github.io/algebird/
 */
case class DoubleField() extends Field[Double] {
  val zero = 0.0
  val one = 1.0
  def plus(l:Double,r:Double):Double = l+r
  def negate(v:Double):Double = -v
  def minus(l:Double, r:Double):Double = l-r
  def times(l:Double,r:Double):Double = l*r
  def divide(l:Double,r:Double):Double = l/r
  def inverse(v:Double):Double = 1/v

}