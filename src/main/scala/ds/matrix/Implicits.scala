package ds.matrix

import ds.expr.Infix.TimesTimes
import ds.expr.Term
import ds.num.Real

object Implicits {

  implicit def times[R:Real]:TimesTimes[Matrix[R]] =
    (em1, em2) => Term("ds.matrix.timesMatrix",em1,em2) { e =>
      Matrix(e(em1).rows map (m1_row => e(em2).columns map (m2_col => e(dot(vec(m1_row),vec(m2_col)))))) //@todo ugly
    }

}
