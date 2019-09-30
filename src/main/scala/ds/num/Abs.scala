package ds.num

import ds.expr.Engine

case class Abs[R:Real](x:E[R])(implicit real:Real[R])  extends E[R] {
  def apply(e:Engine): R = real.abs(e(x))
}
