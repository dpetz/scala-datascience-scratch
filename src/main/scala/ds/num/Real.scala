package ds.num
import parser.{Num, Parser}

/** Abstracts numeric operations from a specifc number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within [0,1) */
  def random:R

  /** Parse from json string. */
  def json(s:String):R

  /** x to the power of y */
  def power(x:R,y:R):R
}


object Real {
  /** Infix operators based on ds.math.Real */
  implicit class Infix[R:Real](x:R) {
    val real = implicitly[Real[R]]
    def **(y: R): R = real.power(x,y)
  }

  /** @toto How to support for BigDecimal? */
  trait NotDefined[R] {
    val NaN: R
    val PositiveInfinity: R
    val NegativeInfinity: R
  }

}
