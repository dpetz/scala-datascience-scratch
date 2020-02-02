package ds.num

import ds.expr.{Engine, Expr, Term}
import ds.expr.Functions.lift

object Functions {

  //def abs[R](implicit real:Real[R]):Expr[R]=>Expr[R] = lift(real.abs)

  def inverse[R:Real](implicit real:Real[R]):Expr[R]=>Expr[R] =
    lift (real.div(real.one, _))

  case class Plus[R:Real](x:Expr[R], y:Expr[R]) extends Term[R] {
    def eval(e:Engine):R = implicitly[Real[R]].plus(e(x),e(y))
  }

}
