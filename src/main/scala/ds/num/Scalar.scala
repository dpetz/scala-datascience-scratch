package ds.num

import ds.expr.Expr

/** Expr evaluating to a real number */
abstract class Scalar[R:Real](implicit real:Real[R]) extends Expr[R] {

  def parts:Seq[Expr[_]] = Nil

  /** @see Real.plus */
  def +(y: E[R]):Expr[R] = real.func.Plus(this,y)

  /** @see Real.minus */
  def -(y: E[R]): Expr[R] = real.func.Minus(this,y)

  /** @see Real.times */
  def *(y: E[R]): Expr[R] =real.func.Times(this,y)

  /** @see Real.div */
  def /(y: E[R]): Expr[R] = real.func.Div(this,y)

  /** @see Real.approx */
  def ~(y: E[R]): Expr[Boolean] = real.func.Approx(this,y)

  /** @see Real.power */
  def **(y: E[R]): Expr[R] = real.func.Power(this,y)

  def abs: Expr[R] = real.func.Abs(this)
}
