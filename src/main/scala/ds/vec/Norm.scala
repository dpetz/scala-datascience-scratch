package ds.vec

import ds.expr.{AbstractComposed, Composed, Expr}
import ds.func
import ds.func.Func
import ds.num.Real
import ds.num._

case class Norm[R](v: Vec[R], p: Expr[R])(implicit real:Real[R]) extends AbstractComposed[R] (e =>
  e(p) match {
    case 1 => v map Func(real.abs) sum
    case p_ => (v map Func(real.power(_,p_)) sum) ** Divide(1,p_)
  })