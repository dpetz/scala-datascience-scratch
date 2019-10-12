package ds.func

import ds.expr.{Engine, Expr}



/** ``Expr`` evaluating to a function with three inputs */
trait F3[X1,X2,X3,Y] extends Func[Y] {

  def eval(e:Engine, x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Y

  /** Assign all inputs */
  def apply(x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Expr[Y] = F3.BindAll(this,x1,x2,x3)
  /** Assign first input */
  def !(x1:Expr[X1]):F2[X2,X3,Y] = F3.Bind1(this,x1)
}


object F3 {

  /** Name and wrap function of three inputs as ``Expr`` */
  def apply[X1,X2,X3,Y](fName:String, func:(Engine,
    Expr[X1], Expr[X2], Expr[X3]) => Y):F3[X1,X2,X3,Y] = new F3[X1,X2,X3,Y] {
    def eval(e:Engine, x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]):Y = func(e,x1,x2,x3)
    val name:String = fName
  }

  /** Bind first input of an [[F3]] */
  case class Bind1[X1,X2,X3,Y](func:F3[X1,X2,X3,Y], x1:Expr[X1]) extends F2[X2,X3,Y]  {
    def name:String = s"$func[$x1]"
    def eval(e:Engine, x2:Expr[X2], x3:Expr[X3]):Y = func.eval(e,x1,x2,x3)
    override def parts = List(func,x1)
  }

  /** Binds function of three arguments to its input. */
  case class BindAll[X1,X2,X3,Y](func:F3[X1,X2,X3,Y], x1:Expr[X1], x2:Expr[X2], x3:Expr[X3]) extends Expr[Y] {
    def eval(e:Engine):Y=func.eval(e,x1,x2,x3)
    def parts = List(func,x1,x2,x3)
  }

}