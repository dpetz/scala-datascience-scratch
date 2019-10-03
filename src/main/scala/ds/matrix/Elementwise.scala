package ds.matrix

import ds.expr.Engine
import ds.expr.Func.{F1, F2, F3}
import ds.num.Real

/** Zip to matrices and map entries to real */
class Elementwise[R: Real](m1: Matrix[R], m2: Matrix[R])(f: F1[R,R])
  extends F3[F2[R, R,R], Matrix[R], Matrix[R], Matrix[R]] {

  def apply(e: Engine): Seq[Seq[R]] =
    (e(m1) zip (e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f(xx._1, xx._2))))
}
