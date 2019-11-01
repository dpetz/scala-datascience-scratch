package ds.vec

import ds.expr.{Expr, Named, Term}
import ds.vec.Implicits._
import ds.num.Real
import ds.num.Functions.abs
import ds.num.Implicits._

object Functions {

  type E[R] = Expr[R]
  type S[R] = Seq[R]


  def sum[R](v: E[S[R]])(implicit real: Real[R]): E[R] =
    Term("ds.vec.sum", v) { e => e(v).foldLeft(real.zero)(real.plus) }

  def dot[R: Real](v: E[S[R]], w: E[S[R]]): E[R] = Named("ds.vec.dot") {
    sum(v * w)
  }

  def norm[R](p: Int)(v: Vec[R])(implicit real:Real[R]): Expr[R] =
    Named("ds.vec.norm") { p match {
        case 1 => v.each(abs).all(sum[R])
        case p => v.each((x: E[R]) => x ** p).all(sum[R]) ** (1 / p)
    }}
}