package ds.num

import ds.expr.{Computable, Engine, Expr}

/** Abstracts numeric operations from a specifc number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within `[min,max)` */
  def random(min:R=zero,max:R=one):R

  /** Parse from json number. */
  def json(n:parser.Num):R

  /** x to the power of y */
  def power(x:R,y:R):R

  /** Convert from double */
  def apply(d:Double):R

  /** Convert from integer */
  def apply(i:Int):R

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

object Real {

  import ds.expr.Expr._

  /** Wrap `Real` functions as infix operators*/
  implicit class RealExpr[R](x:Expr[R]) {

    /** @see Real.power */
    def **(y: E[R]): E[R] = Power(x,y)
    /** @see Real.plus */
    def +(y: E[R]): E[R] = Plus(x,y)
    /** @see Real.minus */
    def -(y: E[R]): E[R] = Minus(x,y)
    /** @see Real.times */
    def *(y: E[R]): E[R] = Times(x,y)
    /** @see Real.div */
    def /(y: E[R]): E[R] = Divide(x,y)
    /** @see Real.approx */
    def ~(y: E[R]): E[Boolean] = Approx(x,y)

  }

  /** Extends ``Real operations, eg. in [[Infix]], to `Int` parameters. */
  implicit def int2Expr[R](x: Int):E[R] = IntExpr(x)
  /** Extends ``Real operations, eg. in [[Infix]], to `Double` parameters. */
  implicit def double2Expr[R](x: Double):E[R] = DoubleExpr(x)




}
