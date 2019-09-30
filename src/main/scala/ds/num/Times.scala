package ds.num

import ds.expr.{E, Engine, Expr}

case class Times[R:Real](x:E[R],y:E[R]) extends Expr[R] {
  def apply(e:Engine): R = e.real.times(e(x),e(y))
}
