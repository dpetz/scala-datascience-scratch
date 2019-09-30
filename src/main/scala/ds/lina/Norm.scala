package ds.lina

import ds.expr.{Composed, Expr}
import ds.num.Real
import ds.calc.Func._

case class Norm[R: Real](v: Vec[R], p: Expr[R]) extends Composed[R] (e =>
  e(p) match {
    case 1 => v map (_.abs) sum
    case p_ => (v map (_ ** p_) sum) ** (1 / p_)
  })