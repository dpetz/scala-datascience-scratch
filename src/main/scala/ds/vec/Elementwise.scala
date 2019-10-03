package ds.vec

import ds.expr.Engine
import ds.vec.Vec.each
import ds.num.Real

class Elementwise[R:Real](v: Vec[R], w: Vec[R])(f: (R, R) => R) extends Vec[R] {
  def apply(e:Engine):Seq[R] = each(e(v), e(w), f)
  def size:Int = v.size
  lazy val inputs = List(v,w)
}


