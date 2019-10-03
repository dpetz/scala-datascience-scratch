package ds.vec

import ds.expr.{Engine, Expr}
import ds.num.Real

case class Size[R](v: Vec[R])(implicit real: Real[R]) extends Expr[R] {

  def apply(e:Engine):R = real(e(v).size)
  def parts = List(v)
}
