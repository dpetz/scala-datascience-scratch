package ds.num

import ds.expr.Engine

case class Plus[R:Real](x:E[R], y:E[R])(implicit real:Real[R])  extends RealExpr[R] {
  def apply(e:Engine): R = real.plus(e(x),e(y))
  lazy val inputs = List(x,y)
}