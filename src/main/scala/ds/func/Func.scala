package ds.func

import ds.expr.{Engine, Expr, ExprException}
import ds.num.Real

trait Func[I,O] extends Expr[O]

case class RealFunc[R:Real](x:Symbol[R], e:Expr[R]) extends Func[R,R] {
  lazy val inputs = List(x,e)
}

object Func {

  /** Wrap scala function as expression. */
  def apply[X,T](f:X=>T):Expr[X=>T]= _ => f

  def apply[R:Real](f:R=>Expr[R]): Func[R,R] = {
    val x = Symbol[R]("x")
    RealFunc(x, f(x))
  }

  /** Auto convert expressions with single free variable */
  def apply[R:Real](e:Expr[R]): Func[R,R] = e.free match {
    case Nil => throw ExprException("No free symbol to create function", e)
    case sym :: Nil => RealFunc(sym,e)
    case _ => throw ExprException("Cannot create function from expression with more than one free symbol", e)
  }
}
