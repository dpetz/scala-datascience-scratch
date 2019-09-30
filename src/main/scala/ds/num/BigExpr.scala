package ds.num

import ds.expr.Engine

case class BigExpr[R: Real](x: BigDecimal)(implicit real:Real[R])  extends E[R] {
  def apply(e: Engine): R = real(x)
}

