package ds.num

import ds.expr.Expr

/** Must be extended by all expressions evaluating to a real number
  * to provide infix operation, such as ``+`` */
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
