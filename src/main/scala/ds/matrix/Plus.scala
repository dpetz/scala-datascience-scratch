package ds.matrix

import ds.num.Real

/** Add matrices */
case class Plus[R](m1: Matrix[R], m2: Matrix[R])(implicit r: Real[R])
  extends Elementwise(m1, m2)(r.plus)
