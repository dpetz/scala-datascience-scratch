package ds.num

import ds.expr.{Engine, Expr}

case class Closure2[-T1,-T2,+R](func:F2[T1,T2,R], x:Expr[T1], y:Expr[T2])

abstract class Func[+R](val name:String) extends Expr[R]

trait F2[-T1,-T2,+R] extends Func[R] {
  def apply(x:Expr[T1], y:Expr[T2]):Closure2[T1,T2,R] = Closure2(this,x,y)
  def apply(e:Engine, x:R, y:R)

}

case class Plus[R:Real](x:E[R], y:E[R])(implicit real:Real[R])  extends RealExpr[R] {
  def apply(e:Engine): R = real.plus(e(x),e(y))
  lazy val inputs = List(x,y)
}