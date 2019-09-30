package ds.num

import ds.expr.{Engine, Expr}

case class Divide[R:Real](x:E[R],y:E[R])(implicit real:Real[R])  extends Expr[R] {
  def apply(e:Engine): R = real.div(e(x),e(y))
}
