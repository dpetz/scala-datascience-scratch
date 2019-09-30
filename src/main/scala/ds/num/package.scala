package ds

import ds.expr.{Engine, Expr}

package object num {

  type E[R] = Expr[R]

  //implicit class ValInfix[R:Real](x:AnyVal) extends ExprInfix(ValExpr(x))
  //implicit class BigInfix[R:Real](x:BigDecimal) extends ExprInfix(BigExpr(x))
  //implicit class RealInfix[R:Real](x:R) extends ExprInfix(RealExpr(x))

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Big2Expr[R: Real](x: BigDecimal)(implicit real:Real[R])  extends RealExpr[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit class Val2Expr[R: Real](x: AnyVal)(implicit real:Real[R]) extends RealExpr[R] {
    def apply(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class Real2Expr[R: Real](x: R) extends RealExpr[R] {
    def apply(e: Engine): R = x
  }
}

