package ds.vec

import ds.expr._
import ds.func.F1
import ds.num._

case class Norm[R](v: Vec[R], p: Expr[R])(implicit real:Real[R])
  extends RealInfix[R] with Expressible[R] {

  def parts:Seq[Expr[_]] = List(v, p)

  def expr(e: Engine): Expr[R] = e(p) match {
    case 1 => (v map F1(real.abs _)) sum
    case p_ => (v map F1(Power('_', p_)) sum) ** Divide(1, p_)
  }
}


