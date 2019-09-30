package ds.vec

import ds.expr.Composed

case class Dot[R](v: Vec[R], w: Vec[R]) extends Composed[R](
  _ => (v * w) sum
)