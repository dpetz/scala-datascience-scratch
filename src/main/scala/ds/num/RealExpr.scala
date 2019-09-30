package ds.num

import ds.expr.{Engine, Expr}

case class RealExpr[R: Real](x: R) extends Expr[R] {
  def apply(e: Engine): R = x
}
