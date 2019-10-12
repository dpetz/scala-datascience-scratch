package ds.func

import ds.expr.{Engine, Expr}


/** ``Expr`` evaluating to a function with three inputs */
trait F3[X1,X2,X3,Y] extends Func[Y] {

  def eval[R](e:Engine[R], x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Y

  /** Assign all inputs */
  def apply(x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Closure.C3[X1,X2,X3,Y] = Closure.C3(this,x1,x2,x3)
  /** Assign first input */
  def !(x1:Expr[X1]):F2[X2,X3,Y] = Closure.F3B1(this,x1)
}


object F3 {
  /** Name and wrap function of three inputs as ``Expr`` */
  def apply[X1,X2,X3,Y](fName:String, func:(Engine[_],
    Expr[X1], Expr[X2], Expr[X3]) => Y):F3[X1,X2,X3,Y] = new F3[X1,X2,X3,Y] {
    def eval[R](e:Engine[R], x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Y = func(e,x1,x2,x3)
    val name:String = fName
  }

  /** Bind first input of an [[F3]] */
  case class B1[X1,X2,X3,Y](func:F3[X1,X2,X3,Y], x1:Expr[X1]) extends F2[X2,X3,Y] with Closure[(X2,X3)=>Y] {
    def name:String = s"$func[$x1]"
    def eval(e:Engine[R]):(X2,X3)=>R= (x2,x3) => e(func)(e(x1),x2,x3)
    override def parts = List(func,x1)
  }

}