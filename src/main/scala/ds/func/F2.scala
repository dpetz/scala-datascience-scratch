package ds.func

import ds.expr.{Engine, Expr, Inputs}
import ds.func.F2.Chain

/** ``Expr`` evaluating to a function with two inputs */
trait F2[-X1 <: Expr[_],-X2 <: Expr[_],+Y] extends Func[Y] {
  /** Assign all inputs */
  def apply(x:X1, y:X2):Expr[Y] = F2.BindAll(this,x,y)

  def eval(e:Engine, x1:X1, x2:X2):Y

  /** Evaluate this, then ``f`` */
  def °[T](f:F1[Expr[Y],T]):F2[X1,X2,T] = Chain(this,f)
  /** Assign first input */
  def !(x1:X1):F1[X2,Y] = F2.Bind1(this,x1)
  /** Assign second input */
  def !!(x2:X2):F1[X1,Y] = F2.Bind2(this,x2)
}


object F2 {

  /** Name and wrap function of two inputs as ``Expr`` */
  def apply[X1 <: Expr[_],X2 <: Expr[_],Y](fName:String, func:(Engine, X1, X2) => Y):F2[X1,X2,Y] = new F2[X1,X2,Y] {
    def eval(e:Engine, x1:X1, x2:X2):Y = func(e,x1,x2)
    val name:String = fName
  }

  /** Bind first input of an [[F2]] */
  case class Bind1[X1 <: Expr[_],X2 <: Expr[_],Y](func:F2[X1,X2,Y], x1:X1) extends F1[X2,Y] with Inputs {
    def name:String = s"$func[$x1,]"
    def eval(e:Engine, x2:X2):Y = func.eval(e,x1,x2)
    override def inputs: Seq[Expr[_]] = List(func,x1)
  }

  /** Bind second input of an [[F2]] */
  case class Bind2[X1 <: Expr[_],X2 <: Expr[_],Y](func:F2[X1,X2,Y], x2:X2) extends F1[X1,Y] with Inputs {
    def name:String = s"$func[,$x2]"
    def eval(e:Engine, x1:X1):Y = func.eval(e,x1,x2)
    override def inputs: Seq[Expr[_]] = List(func, x2)
  }

  /** Binds function of two arguments to its input. */
  case class BindAll[X1 <: Expr[_],X2 <: Expr[_],Y]
    (func:F2[X1,X2,Y], x:X1, y:X2) extends Expr [Y] with Inputs {
    def eval(e:Engine):Y=func.eval(e,x,y)
    def inputs = List(func,x,y)
  }

  /** Chain F2 followed by F1 */
  case class Chain[-T1 <: Expr[_],-T2 <: Expr[_],-R1,+R2](f:F2[T1,T2,R1],g:F1[Expr[R1],R2]) extends F2[T1,T2,R2] {
    def eval(e:Engine,x:T1,y:T2):R2 = g.eval(e,Expr(f.eval(e,x,y)))
    def name:String = "chain"
  }

}
