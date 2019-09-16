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

  /* Convert from double */
  def apply(d:Double):R

}

object Real {

  /*
  implicit class DoubleAsReal[R](x:Double)(implicit real:Real[R]) {
    def toDouble:Double=real.double(x):Double
  }
    */

  /** Infix operators based on ds.math.Real */
  implicit class RealInfix[R:Real] (x:R) (implicit real:Real[R]) {

    def **(y: R): R = real.power(x,y)
    def +(y: R): R = real.minus(x,y)
    def -(y: R): R = real.minus(x,y)
    def *(y: R): R = real.times(x,y)
    def /(y: R): R = real.div(x,y)


    def **(y: Double): R = **(real(y))
    def +(y: Double): R = this.+(real(y))
    def -(y: Double): R = this.-(real(y))
    def *(y: Double): R = *(real(y))
    def /(y: Double): R = /(real(y))

  }

  /** @toto How to support for BigDecimal? */
  trait NotDefined[R] {
    val NaN: R
    val PositiveInfinity: R
    val NegativeInfinity: R
  }

}
