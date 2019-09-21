package ds.expr

import ds.expr.Expr._
import ds.num.Real

/** Engine for `Real` arithmetic*/
class RealEngine[R](implicit real:Real[R]) extends Engine[R]{

  def apply(expr:E[Boolean]): Boolean = expr match {

    case _ => throw new IllegalArgumentException(s"Engine $this cannot evaluate $expr")
  }

  def apply(expr:E[R]):R = expr match {
    case a:Arithmetic[R] => a.f(real)(this(a.x),this(a.y))


    case IntExpr(x) => real(x)
    case DoubleExpr(x) => real(x)

    case c:Computable[R] => c.compute(this)
    case _ => throw new IllegalArgumentException(s"Engine $this cannot evaluate $expr")
  }

}
