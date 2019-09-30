package ds.lina

import ds.expr.{Engine, Expr}
import ds.num.Real

/** Map function expression to each ``Vec`` element. */
case class Map[R: Real](v: Vec[R])(f: Expr[(R=>R)]) extends Vec[R] {
  def apply(e:Engine):Seq[R] = e(v).map(e(f))
}