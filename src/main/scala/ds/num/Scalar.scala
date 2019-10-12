package ds.num

import ds.func.{F1, F2}
import ds.expr.Expr


/** Expr evaluating to a real number */
abstract class Scalar[R](implicit real:Real[R]) extends Expr[R] {

  private val sf = Scalar.functions(real)

  def parts:Seq[Expr[_]] = Nil

  /** @see Real.plus */
  def +(y: E[R]):Expr[R] = sf.plus(this,y)

  /** @see Real.minus */
  def -(y: E[R]): Expr[R] = sf.minus(this,y)

  /** @see Real.times */
  def *(y: E[R]): Expr[R] = sf.times(this,y)

  /** @see Real.div */
  def /(y: E[R]): Expr[R] = sf.div(this,y)

  /** @see Real.approx */
  def ~(y: E[R]): Expr[Boolean] = sf.approx(this,y)

  /** @see Real.power */
  def **(y: E[R]): Expr[R] = sf.power(this,y)

  def abs: Expr[R] = sf.abs(this)
}

object Scalar {

  def functions[R:Real] : Scalar.Functions[R] = new Functions // @todo buffer

  class Functions[R:Real] {
    val plus   : F2[R, R, R]       = F2("+", (e, x1, x2) => e.real[R].plus(e(x1),e(x2)))
    val minus  : F2[R, R, R]       = F2("-", (e, x1, x2) => e.real[R].minus(e(x1),e(x2)))
    val times  : F2[R, R, R]       = F2("*", (e, x1, x2) => e.real[R].times(e(x1),e(x2)))
    val div    : F2[R, R, R]       = F2("/", (e, x1, x2) => e.real[R].div(e(x1),e(x2)))
    val approx : F2[R, R, Boolean] = F2("~", (e, x1, x2) => e.real[R].approx(e(x1),e(x2)))
    val power  : F2[R, R, R]       = F2("**", (e, x1, x2) => e.real[R].power(e(x1),e(x2)))
    val abs    : F1[R, R]          = F1("+", (e, x) => e.real[R].abs(e(x)))
    val negate : F1[R, R]          = F1("-", (e, x) => e.real[R].negate(e(x)))
  }

}
