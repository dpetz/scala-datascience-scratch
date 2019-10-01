package ds.func

import ds.expr.{Engine, Expr}
import ds.num.Real

case class Func[R:Real](x:Var[R], e:Expr[R]) extends Expr[R]

object Func {
  def apply[X,T](f:X=>T):Expr[X=>T]= _ => f
}
