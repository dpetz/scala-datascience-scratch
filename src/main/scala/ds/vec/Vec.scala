package ds.vec

import ds.expr.{Expr, Term}
import ds.num.Real
import ds.num.Implicits._
import ds.expr.Implicits._



/** Wraps a ``Seq[X]`` for infix notations. */
case class Vec[X](ex:Expr[Seq[X]]) {

  type E[R] = Expr[R]
  type S[R] = Seq[R]



  /** map */
  def each[Y](f:E[X]=>E[Y]):E[S[Y]] =
    Term("ds.vec.each",ex,f) { e => e(ex) map (x => e(f(expr(x)))) }

  //def each[Y](f:Y=>E[Y]):E[S[Y]] =
  //  Term("each",ex,f) { e => e(ex) map (x => e(f(x))) }

  /** reduce */
  def all[Y](f:E[Seq[X]]=>E[Y]):E[Y] = f(ex)

  /** Zip and apply binary operation */
  def zip[Y,Z](expr_w:E[S[Y]])(f:(E[X],E[Y])=>E[Z]): Term[S[Z]] =
    Term("ds.vec.zip",ex,expr_w)(e => ( e(ex) zip e(expr_w) ) map (pair => e(f(pair._1,pair._2))))

  def dot(w:Vec[X])(implicit real:Real[X]):Expr[X] = Functions.dot(ex, w.ex)

  //def minBy(f:E[X]=>X)

  /**
    * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
    */
  def update(i:Int, f:E[X]=>E[X]):E[S[X]] =
    Term("ds.vec.update",this,f){ e =>
      e(ex).view.updated(i,e(f(e(ex)(i))))
    }


  def *(x:Expr[X])(implicit real:Real[X]):E[S[X]] = each(_ * x)

  def +(x:Expr[X])(implicit real:Real[X]):E[S[X]] = each(_ + x)


}
