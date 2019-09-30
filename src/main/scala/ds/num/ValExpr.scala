package ds.num

import ds.expr.{Engine, Expr}

case class ValExpr[R: Real](x: AnyVal) extends Expr[R] {
  def apply(e: Engine): R = e.real(x)
}
