package ds

import ds.expr.Expr

package object num {

  type E[R] = Expr[R]

  /** Must be extended by all expressions evaluating to a real number
    * to provide infix operation, such as ``+`` */
  implicit class RealInfix[R: Real](x: E[R]) {

    /** @see Real.plus */
    def +(y: E[R]): Plus[R] = Plus(x, y)

    /** @see Real.minus */
    def -(y: E[R]): Minus[R] = Minus(x, y)

    /** @see Real.times */
    def *(y: E[R]): Times[R] = Times(x, y)

    /** @see Real.div */
    def /(y: E[R]): Divide[R] = Divide(x, y)

    /** @see Real.approx */
    def ~(y: E[R]): Approx[R] = Approx(x, y)

    /** @see Real.power */
    def **(y: E[R]): Power[R] = Power(x, y)
  }

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Expr[R: Real](x: BigDecimal): BigExpr[R] = BigExpr(x)

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def val2Expr[R: Real](x: Double): ValExpr[R] = ValExpr(x)

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def real2Expr[R: Real](x: R): RealExpr[R] = RealExpr(x)
}

