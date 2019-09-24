package ds.expr

import ds.expr.Expr._
import ds.num.Real

/** Engine for `Real` arithmetic*/
class RealEngine[R](implicit real:Real[R]) extends Engine[R]{


  def apply(expr:E[Boolean]):Boolean = expr match {
    case a:Relation[R] => a.f(real)(this(a.x),this(a.y))
    case _ => throw EngineException(this,expr)
  }

  def apply(expr:E[R]):R = expr match {
    case a:Arithmetic[R] => a.f(real)(this(a.x),this(a.y))
    case b:BigDecimalExpr[R] => real(b.x)
    case i:IntExpr[R] => real(i.x)
    case i:DoubleExpr[R] => real(i.x)
    case c:Computable[R] => c.compute(this)
    case _ => throw EngineException(this,expr)
  }

}

case class EngineException[R](eng:Engine[R], expr:Expr[R]) extends Exception {
  override def toString = s"Engine $this cannot evaluate $expr"
}
