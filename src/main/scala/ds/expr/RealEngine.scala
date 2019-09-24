package ds.expr

import ds.lina.Vec
import ds.num.real._


/** Engine for `Real` arithmetic*/
class RealEngine[R:Real](implicit real:Real[R]) extends Engine[R]{

  def apply(v:Vec[R]): Seq[R] = v.eval(this)

  def apply(expr:E[Boolean]):Boolean = expr match {
    case r:Relation[R] => r.eval(this)
    case _ => throw EngineException(this,expr)
  }

  def apply(expr:E[R]):R = expr match {
    case r:Really[R] => r.eval(this)
    case c:Composed[R] => this(c.expr())
    case _ => throw EngineException(this,expr)
  }

}

case class EngineException[R](eng:Engine[R], expr:Expr[R]) extends Exception {
  override def toString = s"Engine $this cannot evaluate $expr"
}
