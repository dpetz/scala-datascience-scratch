package ds.vec

import ds.expr.{Composed, Engine, Expr}
import ds.num.{Real, RealExpr}

case class Dot[R:Real](v: Vec[R], w: Vec[R]) extends RealExpr[R] with Composed[R] {

  def expr(e:Engine): Expr[R] = (v * w) sum

}