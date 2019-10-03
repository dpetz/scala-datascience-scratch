package ds.vec

import ds.expr.Func.{F2, F3}
import ds.expr.{Engine, Func}
import ds.vec.Vec.each
import ds.num.Real

class Elementwise[R:Real] extends F3[F2[R,R,R],Vec[R],Vec[R],Vec[R]]("Elementwise",
  {  e => (f:F2[R,R,R],v:Seq[R],w:Seq[R]) => (v zip w) map (x => f.f(x._1, x._2)) }
) with Vec[R] {
  def apply(e:Engine):Seq[R] = each(e(v), e(w), f)
  def size:Int = v.size
  lazy val parts = List(v,w)
}


