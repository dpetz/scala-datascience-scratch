package ds.num

import ds.expr.{Engine, Expr}

case class ValExpr[R: Real](x: AnyVal)(implicit real:Real[R]) extends Expr[R] {
  def apply(e: Engine): R = real(x)
}
