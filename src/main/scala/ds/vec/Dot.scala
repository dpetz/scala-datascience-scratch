package ds.vec

import ds.expr.Composed
import ds.num.{Real, RealExpr}

case class Dot[R:Real](v: Vec[R], w: Vec[R]) extends RealExpr[R] with Composed[R] {

  def expression:E[]

}
  _ => (v * w) sum
)