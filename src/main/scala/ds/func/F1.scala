package ds.func

import ds.expr.{Engine, Expr}
import ds.func.F1.BindAll


/** ``Expr`` evaluating to a function with one input */
trait F1[-X,+Y] extends Func[Y] {
  def apply(x:Expr[X]):Expr[Y] = BindAll(this,x)
  def eval(e:Engine, x:Expr[X]):Y
  /** Evaluate this, then ``f`` */
  def °[Y2](f:F1[Y,Y2]):F1[X,Y2] = Chain.F1F1(this,f)
}


object F1 {

  /** Binds function of on argument to its input. */
  case class BindAll[X1,Y](func:F1[X1,Y], x:Expr[X1]) extends Expr[Y] {
    def eval(e:Engine):Y=func.eval(e,x)
    def parts = List(func,x)
  }

  /** Name and wrap function of one input as ``Expr`` */
  def apply[X,Y](fName:String, func:(Engine, Expr[X]) => Y):F1[X,Y] = new F1[X,Y] {
    def eval(e:Engine, x:Expr[X]):Y = func(e,x)
    val name:String = fName
  }
}