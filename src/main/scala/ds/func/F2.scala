package ds.func

import ds.expr.{Engine, Expr}

/** ``Expr`` evaluating to a function with two inputs */
trait F2[X1,X2,Y] extends Func[Y] {
  /** Assign all inputs */
  def apply(x:Expr[X1], y:Expr[X2]):Closure.C2[X1,X2,Y] = Closure.C2(this,x,y)

  def eval[R](e:Engine[R], x1:Expr[X1], x2:Expr[X2]):Y

  /** Evaluate this, then ``f`` */
  def °[T](f:F1[Y,T]):F2[X2,X2,T] = Chain.F2F1(this,f)
  /** Assign first input */
  def !(x1:Expr[X1]):F1[X2,Y] = Closure.F2B1(this,x1)
  /** Assign second input */
  def !!(x2:Expr[X2]):F1[X1,Y] = Closure.F2B2(this,x2)
}


object F2 {
  

  /** Name and wrap function of two inputs as ``Expr`` */
  def apply[X1,X2,Y](fName:String, func:(Engine[_], Expr[X1], Expr[X2]) => Y):F2[X1,X2,Y] = new F2[X1,X2,Y] {
    def eval[R](e:Engine[R], x1:Expr[X1], x2:Expr[X2]):Y = func(e,x1,x2)
    val name:String = fName
  }

}
