package ds.matrix

import ds.expr.Engine
import ds.expr.Engine._
import ds.num.Real
import ds.vec.Vec
import parser.Json


/** Implements [[Matrix]] as a vector (the rows) of vectors (the entries).  */
 class SeqOfSeq[R:Real](data:Seq[Seq[R]], rows:Boolean) extends Matrix[R]() {

  val shape = Shape(
    if(rows) data.size else data.head.size,
    if(rows) data.head.size else data.size
  )

  /** All row vectors to have equal length */
  require (data.forall( _.size == shape.rows),
    s"${shape.cols} elements expected: ${data.find(_.size != shape.cols).get}"
  )

  def apply(e: Engine): Seq[Seq[R]] = {

    // Filter

    val filter_dim1 = e.config[Filter](if (rows) ROW_FILTER else COLUMN_FILTER)
    val filter_dim2 = e.config[Filter](if (rows) COLUMN_FILTER else ROW_FILTER)

    data.zipWithIndex.filter(row_i => filter_dim1(row_i._2)).map( // filter rows
      _._1.zipWithIndex.filter(col_j => filter_dim2(col_j._2)).map(_._1)) // filter columns within each row

    // Layout
    // @todo lazy transpose?
    e.config[Layout](MATRIX_LAYOUT) match {
      case _:Rows => if (rows) data else data.transpose
      case _:Columns => if (rows) data.transpose else data//
      case _ => throw new UnsupportedOperationException("Matrix Layout not 'Rows' or 'Columns'.")
    }

  }
}

object SeqOfSeq {
  apply(data:Seq[Seq[R]], )
}
