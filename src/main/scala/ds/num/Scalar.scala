package ds.num

import ds.expr.Func.{F1, F2}
import ds.expr.{Engine, Expr, Func}
import ds.func.Func


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

  def functions[R](implicit r:Real[R]) : Scalar.Functions[R
  = new Functions(r) // @todo buffer]

  class Functions[R:Real] {
    val plus   : F2[R, R, R]       = Func("+", real.plus)
    val minus  : F2[R, R, R]       = Func("-", real.minus)
    val times  : F2[R, R, R]       = Func("*", real.times)
    val div    : F2[R, R, R]       = Func("/", real.div)
    val approx : F2[R, R, Boolean] = Func("~", real.approx)
    val power  : F2[R, R, R]       = Func("**",real.power)
    val abs    : F1[R, R]          = Func[R,R]("+", (e, x) => e.real.abs(e(x)))
    val negate : F1[R, R]          = Func("-", real.negate)
  }

}
