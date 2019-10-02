package ds.vec

import ds.expr.Engine
import ds.func.Func
import ds.num.Real

/** Map function expression to each ``Vec`` element. */
case class MapVec[R: Real](v: Vec[R])(f: Func[R]) extends Vec[R] {
  def apply( e:Engine):Seq[R] = e(v).map { e(f,_) }
}