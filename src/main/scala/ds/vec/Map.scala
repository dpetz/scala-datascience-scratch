package ds.vec

import ds.expr.Engine
import ds.func.F1
import ds.num.Real

/** Map function expression to each ``Vec`` element. */
case class Map[R: Real](v: Vec[R])(f: F1[R,R]) extends Vec[R] {
  def apply( e:Engine):Seq[R] = e(v).map { e(f,_) }
  def size: Int = v.size
  /** Returns vector and function. */
  lazy val inputs = List(v,f)

}