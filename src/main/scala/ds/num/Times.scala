package ds.num

import ds.expr.{Engine, Expr}

case class Times[R:Real](x:Expr[R],y:E[R])(implicit real:Real[R]) extends RealExpr[R] {
  def apply(e:Engine): R = real.times(e(x),e(y))
  lazy val inputs = List(x,y)
}
