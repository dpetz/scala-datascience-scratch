package ds.vec

import ds.expr.{Composed, Engine, Expr}
import ds.num.Real

case class Negate[R:Real](v: Vec[R]) extends Vec[R] with Composed[Seq[R]] {
  def expression(e:Engine):Expr[Seq[R]] = -v
  def apply(e:Engine):Seq[R] = e(-v)
}



