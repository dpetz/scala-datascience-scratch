package ds.vec

import ds.expr._
import ds.num.Real
import ds.num._

case class Norm[R](v: Vec[R], p: Expr[R])(implicit real:Real[R])
  extends RealExpr[R] with Expressible[R] {

  def expr(e:Engine):Expr[R] =  e(p) match {
    case 1 => v map Func(real.abs) sum
    case p_ => (v map Power('_',p_)) sum) ** Divide(1,p_) // Func(real.power(_,p_))


}


