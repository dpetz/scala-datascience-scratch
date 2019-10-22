package ds.matrix

import ds.expr.Infix.TimesTimes
import ds.vec.Implicits._
import ds.expr.Term
import ds.num.Real

import ds.vec.Functions.dot

object Implicits {

  implicit def times[R:Real]:TimesTimes[Matrix[R]] =
    (em1, em2) => Term("ds.matrix.timesMatrix",em1,em2) { e =>
      Matrix(e(em1).rows map (m1_row => e(em2).columns map (m2_col => e(dot(seq2Expr(m1_row),seq2Expr(m2_col)))))) //@todo ugly
    }

}
