package ds.lina

import ds.num.Real

case class Plus[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
  extends Elementwise[R](v, w)(r.plus)
