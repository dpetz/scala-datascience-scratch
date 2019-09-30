package ds.num

import ds.expr.{Engine, Expr}

case class Power[R:Real](x:E[R],y:E[R])(implicit real:Real[R])  extends Expr[R] {
  def apply(e:Engine): R = real.power(e(x),e(y))
}
