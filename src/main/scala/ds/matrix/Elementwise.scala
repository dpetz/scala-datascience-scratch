package ds.matrix

import ds.expr.Engine
import ds.num.Real

/** Zip to matrices and map entries to real */
class Elementwise[R: Real](m1: Matrix[R], m2: Matrix[R])(f: (R, R) => R)
  extends Matrix[R](m1.shape) {

  def apply(e: Engine): Seq[Seq[R]] =
    (e(m1) zip (e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f(xx._1, xx._2))))
}
