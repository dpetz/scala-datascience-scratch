package ds.matrix

import ds.expr.Engine
import ds.expr.Engine.Layout
import ds.num.Real

/** Transpose matrix */
private case class Transpose[R: Real](m: Matrix[R]) extends Matrix[R](m.shape.transpose) {
  def apply(e: Engine): Seq[Seq[R]] = e.update(e.config[Layout]("Layout"))(m)
}
