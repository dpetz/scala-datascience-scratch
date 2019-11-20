package ds.vec

import ds.expr.{Engine, Expr, Expressible, Named, Term}
import ds.vec.Implicits._
import ds.num.Real
import ds.num.Functions.abs
import ds.num.Implicits._

object Functions {

  def sum[R](v: Vec[R])(implicit real: Real[R]): Expr[R] =
    Term("ds.vec.sum")(v) { e => e(v).foldLeft(real.zero)(real.plus) }

  def dot[R: Real](v: Vec[R], w: Vec[R]): Expr[R] =
    Named("ds.vec.dot") { sum(v * w) }

  def norm[R](p: Int)(v: Vec[R])(implicit real:Real[R]): Expr[R] =
    Named("ds.vec.norm") { p match {
        case 1 => v.each(abs).all(sum[R])
        case p => v.each((x: Expr[R]) => x ** p).all(sum[R]) ** (1 / p)
    }}
}