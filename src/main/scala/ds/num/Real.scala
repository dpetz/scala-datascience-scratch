package ds.num

/** Abstracts numeric operations from a specifc number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within [0,1) */
  def random:R

  /** Parse from json number. */
  def json(n:parser.Num):R

  /** x to the power of y */
  def power(x:R,y:R):R

  /** Convert from double */
  def apply(d:Double):R

  /** Convert from integer */
  def apply(i:Int):R

}

object Real {

  /** Infix operators based on ds.math.Real */
  implicit class RealInfix[R:Real](x:R)(implicit real:Real[R]) {

    def **(y: R): R = real.power(x,y)
    def +(y: R): R = real.minus(x,y)
    def -(y: R): R = real.minus(x,y)
    def *(y: R): R = real.times(x,y)
    def /(y: R): R = real.div(x,y)
  }

  /** Overload [[RealInfix]] operations by converting [[Double]] parameters to [[R]]
    * @see Real.apply(Double)
    */
  implicit class DoubleInfix [R:Real] (x:R) (implicit real:Real[R]) {

    def **(y: Double): R = real.power(x,real(y))
    def +(y: Double): R = real.plus(x,real(y))
    def -(y: Double): R = real.minus(x,real(y))
    def *(y: Double): R = real.times(x,real(y))
    def /(y: Double): R = real.div(x,real(y))
  }

  /** Overload [[RealInfix]] operations by converting [[Int]] parameters to [[R]]
    * @see Real.apply(Int)
    */
  implicit class IntInfix [R:Real] (x:R) (implicit real:Real[R]) {

    def **(y: Int): R = real.power(x,real(y))
    def +(y: Int): R = real.plus(x,real(y))
    def -(y: Int): R = real.minus(x,real(y))
    def *(y: Int): R = real.times(x,real(y))
    def /(y: Int): R = real.div(x,real(y))
  }

  /** @toto How to support for BigDecimal? */
  trait NotDefined[R] {
    val NaN: R
    val PositiveInfinity: R
    val NegativeInfinity: R
  }

}
