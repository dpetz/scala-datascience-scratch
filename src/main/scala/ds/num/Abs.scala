package ds.num

import ds.expr.Engine

case class Abs[R:Real](x:E[R])(implicit real:Real[R])  extends RealExpr[R] {
  def apply(e:Engine): R = real.abs(e(x))
  lazy val inputs = List(x)
}
