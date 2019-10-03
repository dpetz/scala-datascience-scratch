package ds.num

import ds.expr.Engine

case class Minus[R:Real](x:E[R],y:E[R])(implicit real:Real[R]) extends RealExpr[R] {
  def apply(e:Engine): R = real.minus(e(x),e(y))
  lazy val inputs = List(x,y)
}