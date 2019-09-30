package ds.lina

import ds.expr.Engine
import ds.lina.Matrix.{M, SS}
import ds.lina.Vec.each
import ds.num.Real

object Elementwise {

  class ElementwiseVec[R:Real](v: Vec[R], w: Vec[R])(f: (R, R) => R) extends Vec[R] {
    def apply(e:Engine):Seq[R] = each(e(v), e(w), f)
  }


  /** Zip to matrices and map entries to real */
  class ElementwiseMat[R: Real](m1: M[R], m2: M[R])(f: (R, R) => R) extends M[R](m1.shape) {
    def apply(e: Engine): SS[R] =
      (e(m1) zip (e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f(xx._1, xx._2))))
  }

}