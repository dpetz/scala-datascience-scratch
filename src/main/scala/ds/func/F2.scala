package ds.func

import ds.expr.{Engine, Expr}

/** ``Expr`` evaluating to a function with two inputs */
trait F2[X1,X2,Y] extends Func[Y] {
  /** Assign all inputs */
  def apply(x:Expr[X1], y:Expr[X2]):Expr[Y] = F2.BindAll(this,x,y)

  def eval(e:Engine, x1:Expr[X1], x2:Expr[X2]):Y

  /** Evaluate this, then ``f`` */
  def °[T](f:F1[Y,T]):F2[X2,X2,T] = Chain.F2F1(this,f)
  /** Assign first input */
  def !(x1:Expr[X1]):F1[X2,Y] = F2.Bind1(this,x1)
  /** Assign second input */
  def !!(x2:Expr[X2]):F1[X1,Y] = F2.Bind2(this,x2)
}


object F2 {

  /** Name and wrap function of two inputs as ``Expr`` */
  def apply[X1,X2,Y](fName:String, func:(Engine, Expr[X1], Expr[X2]) => Y):F2[X1,X2,Y] = new F2[X1,X2,Y] {
    def eval(e:Engine, x1:Expr[X1], x2:Expr[X2]):Y = func(e,x1,x2)
    val name:String = fName
  }

  /** Bind first input of an [[F2]] */
  case class Bind1[X1,X2,X3,Y](func:F2[X1,X2,Y], x1:Expr[X1]) extends F1[X2,Y]  {
    def name:String = s"$func[$x1,]"
    def eval(e:Engine, x2:Expr[X2]):Y = func.eval(e,x1,x2)
    override def parts = List(func,x1)
  }

  /** Bind second input of an [[F2]] */
  case class Bind2[X1,X2,X3,Y](func:F2[X1,X2,Y], x2:Expr[X2]) extends F1[X1,Y]  {
    def name:String = s"$func[,$x2]"
    def eval(e:Engine, x1:Expr[X1]):Y = func.eval(e,x1,x2)
    override def parts = List(func,x2)
  }

  /** Binds function of two arguments to its input. */
  case class BindAll[X1,X2,Y](func:F2[X1,X2,Y], x:Expr[X1], y:Expr[X2]) extends Expr [Y] {
    def eval(e:Engine):Y=func.eval(e,x,y)
    def parts = List(func,x,y)
  }

}
