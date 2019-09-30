package ds.func

import ds.expr.{Engine, Expr}

object Func {
  def apply[X,T](f:X=>T):Expr[X=>T]= (e: Engine) => f
}
