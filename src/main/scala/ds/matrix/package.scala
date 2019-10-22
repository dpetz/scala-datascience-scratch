package ds

import ds.expr._
import ds.vec._
import ds.num.Real

package object matrix {

  type M[T] = Matrix[T]
  type E[T] = Expr[T]

  def rows[T](data:Seq[Seq[T]]):E[M[T]] = Term("ds.matrix.rows",data) { _ => new Matrix[T] {
      validateShape(data)
      def rows:SS = data
      def columns:SS = data.transpose //@todo: view instead copy
   }
  }

  def columns[T](data:Seq[Seq[T]]):E[M[T]] = Term("ds.matrix.columns",data) { _ => new Matrix[T] {
      validateShape(data)
      def rows:SS = data.transpose //@todo: view instead copy
      def columns:SS = data
    }
  }

  implicit def timesMatrix[R](implicit real:Real[R]):TimesTimes[M[R],M[R],M[R]] =
    (em1, em2) => Term("ds.matrix.timesMatrix",em1,em2) { e =>
      Matrix(e(em1).rows map (m1_row => e(em2).columns map (m2_col => e(dot(m1_row, m2_col)))))
    }

}
