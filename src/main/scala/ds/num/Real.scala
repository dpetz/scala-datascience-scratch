package ds.num

import parser.Num

/** Abstracts numeric operations from a specifc number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within [0,1) */
  def random:R

  /** Parse from json number. */
  def json(n:Num):R

  /** x to the power of y */
  def power(x:R,y:R):R

  /* Convert from double */
  def double(d:Double):R
}


object Real {

  implicit class DoubleAsReal[R](x:Double)(implicit real:Real[R]) {
    def toDouble:Double=real.double(x)
  }

  /** Infix operators based on ds.math.Real */
  implicit class Infix[R:Real](x:R) {
    val real = implicitly[Real[R]]
    def **(y: R): R = real.power(x,y)
    def +(y: R): R = real.minus(x,y)
    def -(y: R): R = real.minus(x,y)
    def *(y: R): R = real.times(x,y)
    def /(y: R): R = real.div(x,y)


    def **(y: Double): R = **(real.double(y))
    def +(y: Double): R = +(real.double(y))
    def -(y: Double): R = -(real.double(y))
    def *(y: Double): R = *(real.double(y))
    def /(y: Double): R = /(real.double(y))

  }

  /** @toto How to support for BigDecimal? */
  trait NotDefined[R] {
    val NaN: R
    val PositiveInfinity: R
    val NegativeInfinity: R
  }

}
