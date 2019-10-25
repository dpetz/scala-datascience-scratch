package ds.vec

import ds.expr.{Expr, Term}
import ds.num.Real
import ds.expr.Implicits._

case class Vec[X](expr_v:Expr[Seq[X]]) {

  type E[R] = Expr[R]
  type S[R] = Seq[R]



  /** map */
  def each[Y](f:E[X]=>E[Y]):E[S[Y]] =
    Term("each",expr_v,f) { e => e(expr_v) map (x => e(f(expr(x)))) }

  //def each[Y](f:Y=>E[Y]):E[S[Y]] =
  //  Term("each",expr_v,f) { e => e(expr_v) map (x => e(f(x))) }

  /** reduce */
  def all[Y](f:E[Seq[X]]=>E[Y]):E[Y] = f(expr_v)

  /** Zip and apply binary operation */
  def zip[Y,Z](expr_w:E[S[Y]])(f:(E[X],E[Y])=>E[Z]): Term[S[Z]] =
    Term("ds.vec.zip",expr_v,expr_w)(e => ( e(expr_v) zip e(expr_w) ) map (pair => e(f(pair._1,pair._2))))

  /**
    * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
    */
  def update(i:Int, f:E[X]=>E[X]):E[S[X]] =
    Term("ds.vec.update",this,f){ e =>
      e(expr_v).view.updated(i,e(f(e(expr_v)(i))))
    }


  def *(x:Expr[X])(implicit real:Real[X]):E[S[X]] = each(_ * x)

  def +(x:Expr[X])(implicit real:Real[X]):E[S[X]] = each(_ + x)


}
