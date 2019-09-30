package ds.vec

import ds.vec.Vec.E
import ds.num.Real

case class Size[R](v: Vec[R])(implicit r: Real[R]) extends E[R](
  e => r(e(v).size)
)
