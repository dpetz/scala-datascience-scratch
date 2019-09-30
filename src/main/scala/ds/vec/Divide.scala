package ds.vec

import ds.expr.{Composed, Engine, Expr}
import ds.vec.Vec.E
import ds.num.Real

case class Divide[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
  extends Elementwise[R](v, w)(r.div)








