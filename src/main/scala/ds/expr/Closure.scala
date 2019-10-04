package ds.expr

import ds.expr.Func.{F1, F2, F3}

trait Closure[R] extends Expr[R]

object Closure {

  /** Binds function of on argument to its input. */
  case class C1[T1,R](func:F1[T1,R], x:Expr[T1]) extends Closure[R] {
    def apply(e:Engine):R=e(func)(e(x))
    def parts = List(func,x)
  }

  /** Binds function of two arguments to its input. */
  case class C2[T1,T2,R](func:F2[T1,T2,R], x:Expr[T1], y:Expr[T2]) extends Closure[R] {
    def apply(e:Engine):R=e(func)(e(x),e(y))
    def parts = List(func,x,y)
  }

  /** Binds function of three arguments to its input. */
  case class C3[T1,T2,T3,R](func:F3[T1,T2,T3,R], x1:Expr[T1], x2:Expr[T2], x3:Expr[T3]) extends Closure[R] {
    def apply(e:Engine):R=e(func)(e(x1),e(x2),e(x3))
    def parts = List(func,x1,x2,x3)
  }

}