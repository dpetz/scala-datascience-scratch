package ds.num

import ds.expr.{Engine, Expr}
import ds.func.Assign

/** Infix operations for ``Expr`` evaluating to real numbers.
  * Extend to remove need for implicit conversion via [[ds.num.realExpr]]. */
abstract class RealExpr[R: Real] extends E[R]{

  /** @see Real.plus */
  def +(y: E[R]): Plus[R] = Plus(this, y)

  /** @see Real.minus */
  def -(y: E[R]): Minus[R] = Minus(this, y)

  /** @see Real.times */
  def *(y: E[R]): Times[R] = Times(this, y)

  /** @see Real.div */
  def /(y: E[R]): Divide[R] = Divide(this, y)

  /** @see Real.approx */
  def ~(y: E[R]): Approx[R] = Approx(this, y)

  /** @see Real.power */
  def **(y: E[R]): Power[R] = Power(this, y)
}
