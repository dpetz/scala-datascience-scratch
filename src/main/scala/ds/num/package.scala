package ds

import ds.expr.Func.{F1, F2}
import ds.expr.{Engine, Expr, Func}

package object num {

  type E[R] = Expr[R]

  //implicit class ValInfix[R:Real](x:AnyVal) extends ExprInfix(ValExpr(x))
  //implicit class BigInfix[R:Real](x:BigDecimal) extends ExprInfix(BigExpr(x))
  //implicit class RealInfix[R:Real](x:R) extends ExprInfix(RealExpr(x))

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Big2Expr[R: Real](x: BigDecimal)(implicit real: Real[R]) extends RealInfix[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit class Val2Expr[R: Real](x: AnyVal)(implicit real: Real[R]) extends RealInfix[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Real2Expr[R: Real](x: R) extends RealInfix[R] {
    def apply(e: Engine): R = x
  }

  implicit def realExpr[R:Real](expr: Expr[R]):RealInfix[R] = new RealInfix[R] {
    def apply(e: Engine): R = e(expr)
  }

  /** Defines common functions for real valued ``Expr`` and binds them to their inputs */
  implicit class RealInfix[R](x:Expr[R])(implicit real:Real[R]) {

    /** @see Real.plus */
    def +(y: E[R]):Expr[R] = real.func.Plus(x,y)

    /** @see Real.minus */
    def -(y: E[R]): Expr[R] = real.func.Minus(x,y)

    /** @see Real.times */
    def *(y: E[R]): Expr[R] =real.func.Times(x,y)

    /** @see Real.div */
    def /(y: E[R]): Expr[R] = real.func.Div(x,y)

    /** @see Real.approx */
    def ~(y: E[R]): Expr[Boolean] = real.func.Approx(x,y)

    /** @see Real.power */
    def **(y: E[R]): Expr[R] = real.func.Power(x,y)

    def abs: Expr[R] = real.func.Abs(x)
  }
}