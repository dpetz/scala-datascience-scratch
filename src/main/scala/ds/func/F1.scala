package ds.func

import ds.expr.{Engine, Expr}


/** ``Expr`` evaluating to a function with one input */
trait F1[X,Y] extends Func[Y] {
  def apply(x:Expr[X]):Closure.C1[X,Y] = Closure.C1(this,x)
  def eval[R](e:Engine[R], x:Expr[X]):Y
  /** Evaluate this, then ``f`` */
  def °[Y2](f:F1[Y,Y2]):F1[X,Y2] = Chain.F1F1(this,f)
}


object F1 {

  /** Name and wrap function of one input as ``Expr`` */
  def apply[X,Y](fName:String, func:(Engine[_], Expr[X]) => Y):F1[X,Y] = new F1[X,Y] {
    def eval[R](e:Engine[R], x:Expr[X]):Y = func(e,x)
    val name:String = fName
  }
}