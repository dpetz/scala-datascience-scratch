package ds.expr

import ds.expr.Really._
import ds.num.real._

/** Expression evaluating to a real number. */
class Really[R:Real](val eval:Engine[R] => R) extends E[R] {

  /** @see Real.power */
  def **(y: E[R]): Power[R] = Power(this,y)
  /** @see Real.plus */
  def +(y: E[R]): Plus[R] = Plus(this,y)
  /** @see Real.minus */
  def -(y: E[R]): Minus[R] = Minus(this,y)
  /** @see Real.times */
  def *(y: E[R]): Times[R] = Times(this,y)
  /** @see Real.div */
  def /(y: E[R]): Divide[R] = Divide(this,y)
  /** @see Real.approx */
  def ~(y: E[R]): Approx[R] = Approx(this,y)

}


object Really {

  case class Plus[R:Real](x:E[R],y:E[R]) extends Really[R](e => e(x) + e(y))
  case class Minus[R:Real](x:E[R],y:E[R]) extends Really[R](e => e(x) - e(y))
  case class Times[R:Real](x:E[R],y:E[R]) extends Really[R](e => e(x) * e(y))
  case class Divide[R:Real](x:E[R],y:E[R]) extends Really[R](e => e(x) / e(y))
  case class Power[R:Real](x:E[R],y:E[R]) extends Really[R](e => e(x) ** e(y))

  case class Approx[R:Real](x:E[R],y:E[R]) extends Relation[R](x,y)(e => e(x) ~ e(y))


}