package ds.matrix

import ds.expr.Engine
import ds.num.Real

/** Map matrix entries. */
case class Map[R: Real](m: Matrix[R])(f: Engine => R => R) extends Matrix[R](m.shape) {
  def apply(e: Engine): Seq[Seq[R]] = e(m) map (v => v map f(e))
}
