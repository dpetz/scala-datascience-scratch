package ds.expr

import ds.expr.Func.{F1, F2}

trait Closure[+R] extends Expr[R]


object Closure {

  case class C1[-T1,+R](func:F1[T1,R], x:Expr[T1]) extends Closure[R] {
    def parts = List(func,x)
  }

  case class C2[-T1,-T2,+R](func:F2[T1,T2,R], x:Expr[T1], y:Expr[T2]) extends Closure[R] {
    def parts = List(func,x,y)
  }

}