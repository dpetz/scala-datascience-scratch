package ds

import ds.expr.Infix.TimesTimes
import ds.expr._
import ds.vec._
import ds.num.Real

import parser.{Json, Parser}


package object matrix {

  type M[T] = Matrix[T]
  type E[T] = Expr[T]

  /** Wraps ``data`` as sequence of rows. */
  def rows[T](data:Seq[Seq[T]]):Term[M[T]] = Term("ds.matrix.rows",data) { _ => new Matrix[T] {
      validateShape(data)
      def rows:SS = data
      def columns:SS = data.transpose //@todo: view instead copy
   }
  }

  /** Wraps ``data`` as sequence of columns. */
  def columns[T](data:Seq[Seq[T]]):Term[M[T]] = Term("ds.matrix.columns",data) { _ => new Matrix[T] {
      validateShape(data)
      def rows:SS = data.transpose //@todo: view instead copy
      def columns:SS = data
    }
  }

  /** Parses json array of array of real numbers and wraps as expression via [[rows(Seq[Seq[_])]]. */
  def rows[R:Real](json:String):E[M[R]]  = rows(parse(json))

  /** Parses json array of array of real numbers and wraps as expression via [[columns(Seq[Seq[_])]]. */
  def columns[R:Real](json:String):E[M[R]]  = columns(parse(json))


  /** Parses Json array of arrays into a ``Seq[Seq[R`` */
  private def parse[R](json:String)(implicit real:Real[R]):Seq[Seq[R]] = {

    val parser = Json.Parsers.arrOf(ds.vec.parser)

    Json(json, parser).toArr.values.map {
      _.toArr.values.map {
        j:Json => real.json(j.toNum)
      } toVector
    }.toVector
  }

  implicit def times[R:Real]:TimesTimes[M[R]] =
    (em1, em2) => Term("ds.matrix.timesMatrix",em1,em2) { e =>
      Matrix(e(em1).rows map (m1_row => e(em2).columns map (m2_col => e(dot(m1_row, m2_col)))))
    }

}
