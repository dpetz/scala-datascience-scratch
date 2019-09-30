package ds.vec

import ds.expr.Engine
import ds.vec.Vec.E
import ds.num.Real

case class Sum[R:Real](v: Vec[R])(implicit r: Real[R]) extends E[R] {
  def apply(e:Engine):R = e(v).fold(r.zero)(r.plus)
}
