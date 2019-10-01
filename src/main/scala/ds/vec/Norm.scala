package ds.vec

import ds.expr.{Composed, Engine, Expr}
import ds.func._
import ds.num.Real
import ds.num._

case class Norm[R](v: Vec[R], p: Expr[R])(implicit real:Real[R])
  extends RealExpr[R] with Composed[R] {

  def expr(e:Engine):Expr[R] =  e(p) match {
    case 1 => v map Func(real.abs) sum
    case p_ => (v map ( Power(p) ) sum) ** Divide(1,p_) // Func(real.power(_,p_))
  }

}


case class Map[R: Real](v: Vec[R])(f: Func[R]) extends Vec[R] {
  def apply( e:Engine):Seq[R] = e(v).map { e(f,_) }
}