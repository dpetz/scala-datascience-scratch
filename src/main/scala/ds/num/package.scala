package ds

import ds.expr.Func.{F1, F2}
import ds.expr.{Engine, Expr}

package object num {

  type E[R] = Expr[R]

  //implicit class ValInfix[R:Real](x:AnyVal) extends ExprInfix(ValExpr(x))
  //implicit class BigInfix[R:Real](x:BigDecimal) extends ExprInfix(BigExpr(x))
  //implicit class RealInfix[R:Real](x:R) extends ExprInfix(RealExpr(x))

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Big2Expr[R: Real](x: BigDecimal)(implicit real: Real[R]) extends RealExpr[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit class Val2Expr[R: Real](x: AnyVal)(implicit real: Real[R]) extends RealExpr[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Real2Expr[R: Real](x: R) extends RealExpr[R] {
    def apply(e: Engine): R = x
  }

  implicit def realExpr[R:Real](expr: Expr[R]):RealExpr[R] = new RealExpr[R] {
    def apply(e: Engine): R = e(expr)
  }

  /** Infix operations for ``Expr`` evaluating to real numbers.
    * Extend to remove need for implicit conversion via [[ds.num.realExpr]]. */
  implicit class Infix[R: Real] extends E[R]{


    /** @see Real.plus */
    def +(y: E[R]):Expr[R] = new F2[R,R,R]("+", _.real.plus)

    /** @see Real.minus */
    def -(y: E[R]): Expr[R] = new F2[R,R,R]("-", _.real.minus)

    /** @see Real.times */
    def *(y: E[R]): Expr[R] =new F2[R,R,R]("*", _.real.times)

    /** @see Real.div */
    def /(y: E[R]): Expr[R] = new F2[R,R,R]("/", _.real.div)

    /** @see Real.approx */
    def ~(y: E[R]): Expr[Boolean] = new F2[R,R,Boolean]("~", _.real.approx)

    /** @see Real.power */
    def **(y: E[R]): Expr[R] = new F2[R,R,R]("**", _.real.power)

    def abs: Expr[R] = new F1[R,R]("abs", _.real.abs)
  }
}