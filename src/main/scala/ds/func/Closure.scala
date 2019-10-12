package ds.func

import ds.expr.{Engine, Expr}

trait Closure[Y] extends Expr[Y]

object Closure {

  /** Binds function of on argument to its input. */
  case class C1[X1,Y](func:F1[X1,Y], x:Expr[X1]) extends Closure[Y] {
    def eval[R](e:Engine[R]):Y=func.eval(e,x)
    def parts = List(func,x)
  }

  /** Binds function of two arguments to its input. */
  case class C2[X1,X2,Y](func:F2[X1,X2,Y], x:Expr[X1], y:Expr[X2]) extends Closure[Y] {
    def eval[R](e:Engine[R]):Y=func.eval(e,x,y)
    def parts = List(func,x,y)
  }

  /** Binds function of three arguments to its input. */
  case class C3[X1,X2,X3,Y](func:F3[X1,X2,X3,Y], x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]) extends Closure[Y] {
    def eval[R](e:Engine[R]):Y=func.eval(e,x1,x2,x3)
    def parts = List(func,x1,x2,x3)
  }





}