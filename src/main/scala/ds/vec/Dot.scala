package ds.vec

import ds.expr.{Composed, Engine, Expr}
import ds.num.{Real, RealInfix}

case class Dot[R:Real](v: Vec[R], w: Vec[R]) extends RealInfix[R] with Composed[R] {

  def expr(e:Engine): Expr[R] = (v * w) sum

}