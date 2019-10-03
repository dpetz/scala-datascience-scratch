package ds.vec

import ds.expr.{Engine, Expr, Expressible}
import ds.num.Real

case class Negate[R:Real](v: Vec[R]) extends Vec[R] with Expressible[Seq[R]] {
  def express(e:Engine):Expr[Seq[R]] = -v
  lazy val parts = List(v)

}



