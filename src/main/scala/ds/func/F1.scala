package ds.func

import ds.expr._
import ds.func.F1._

/** ``Expr`` evaluating to a function with one input */
trait F1[-X <: Expr[_],+Y] extends Func[Y] {
  def apply(x:X):Expr[Y] = BindAll(this,x)
  def eval(e:Engine, x:X):Y
  /** Evaluate this, then ``f`` */
  def °[Y2](f:F1[Expr[Y],Y2]):F1[X,Y2] = Chain(this,f)
}

object F1 {

  /** Binds function of on argument to its input. */
  case class BindAll[X1 <: Expr[_],Y](func:F1[X1,Y], x:X1) extends Expr[Y] with Inputs {
    def eval(e:Engine):Y=func.eval(e,x)
    def inputs = List(func,x)
  }

  /** Name and wrap function of one input as ``Expr`` */
  def apply[X <: Expr[_],Y](fName:String, func:(Engine, X) => Y):F1[X,Y] = new F1[X,Y] {
    def eval(e:Engine, x:X):Y = func(e,x)
    val name:String = fName
  }

  /** Chain F1 followed by F1 */
  case class Chain[T1 <: Expr[_],R1,R2](f:F1[T1,R1],g:F1[Expr[R1],R2]) extends F1[T1,R2] with Inputs {
    def eval(e:Engine, x:T1):R2 = g.eval(e,Expr(f.eval(e,x)))
    def name:String = "chain"
    override def inputs = List(f,g)
  }
}