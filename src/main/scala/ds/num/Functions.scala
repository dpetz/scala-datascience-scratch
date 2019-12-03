package ds.num

import ds.expr.Expr
import ds.expr.Functions.lift

object Functions {

  def abs[R](implicit real:Real[R]):Expr[R]=>Expr[R] = lift(real.abs)

  def inverse[R:Real](implicit real:Real[R]):Expr[R]=>Expr[R] =
    lift (real.div(real.one, _))

  case class Plus[R:Real](x:Expr[R], y:Expr[R]) extends TypedTerm

}
