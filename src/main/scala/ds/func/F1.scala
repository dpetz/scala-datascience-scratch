package ds.func

import ds.expr._
import ds.num.Real


case class F1[I,O](x:Symbol[I], e:Expr[O]) extends Expr[O] {
  lazy val inputs = List(x,e)
}

object F1 {

  /** Wrap scala function as expression. */
  def apply[I,O](f:I=>O):F1[I,O] = {
    val s = Symbol[I]("x")
    F1(s, new Expr[O] {
      def apply(e:Engine):O =  f(e(s))
      def inputs = List(s)
    })
  }

  def apply[R:Real](f:R=>Expr[R]): F1[R,R] = {
    val x = Symbol[R]("x")
    F1(x, f(x))
  }

  /** Auto convert expressions with single free variable */
  def apply[I,O](e:Expr[O]): F1[I,O] = e.free match {
    case Nil => throw ExprException("No free symbol to create function", e)
    case sym :: Nil => F1(sym,e)
    case _ => throw ExprException("Cannot create function from expression with more than one free symbol", e)
  }
}
