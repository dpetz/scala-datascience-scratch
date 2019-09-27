package ds.num

import parser.Json

object real {
  /** Abstracts numeric operations from a specifc number representation
    * such as Double or BigDecimal. */
  trait Real[R] extends scala.math.Fractional[R] {

    /** Create random number within `[min,max)` */
    def random(min:R=zero,max:R=one):R

    /** Parse from json number. */
    def json(n:Json.Num):R

    /** x to the power of y */
    def power(x:R,y:R):R

    /** Convert from double */
    def apply(d:Double):R

    /** Convert from integer */
    def apply(i:Int):R

    /** Convert from BigDecimal */
    def apply(b:BigDecimal):R


    /** Tiny constant used in approximations.
      * @see ~, ds.calc.Gradient
      */
    def precision:R

    /** Approximately equal
      * @return Difference is at most [[precision]] */
    def approx(x:R,y:R):Boolean

    /** Huge number treated as largest possible
      * For a [[Double]] it is the actual highest possible number.
      * For [[BigDecimal]] it is treated as if it where for consistency */
    def MAX:R

    /** Opposite of [[MAX]] */
    def MIN:R

  }



  /** Wrap `Real` functions as infix operators*/
  implicit class Infix[R:Real](x:R)(implicit real:Real[R]) {

    /** @see Real.power */
    def **(y: R): R = real.power(x,y)
    /** @see Real.plus */
    def +(y: R): R = real.plus(x,y)
    /** @see Real.minus */
    def -(y: R): R = real.minus(x,y)
    /** @see Real.times */
    def *(y: R): R = real.times(x,y)
    /** @see Real.div */
    def /(y: R): R = real.div(x,y)
    /** @see Real.approx */
    def ~(y: R): Boolean = real.approx(x,y)

    def abs: R = real.abs(x)

    def unary_-(): R = real.negate(x)

  }

  implicit class DoubleInfix[R](d:Double)(implicit real:Real[R]) extends Infix(real(d))

  /** Extends ``Real operations, eg. in [[Infix]], to `Int` parameters. */
  implicit def int2Real[R](x: Int)(implicit real:Real[R]):R = real(x)
  /** Extends ``Real operations, eg. in [[Infix]], to `Double` parameters. */
  implicit def double2Real[R](x: Double)(implicit real:Real[R]):R = real(x)

}