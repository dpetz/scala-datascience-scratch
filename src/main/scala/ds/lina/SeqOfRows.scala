package ds.lina

import ds.expr.Engine
import ds.expr.Engine._
import ds.lina.Matrix.{SS, Shape}
import ds.num.real._
import parser.Json


/** Implements [[Matrix]] as a vector (the rows) of vectors (the entries).  */
case class SeqOfRows[R:Real](data:Seq[Seq[R]]) extends Matrix[R](Shape(data.size, data.head.size)) {

  /** All row vectors to have equal length */
  require (data.forall( _.size == shape.rows),
    s"${shape.cols} elements expected: ${data.find(_.size != shape.cols).get}"
  )

  def eval(e: Engine[R]): SS[R] = {

    // Filter

    val row_filter = e.config[Filter](ROW_FILTER)
    val col_filter = e.config[Filter](COLUMN_FILTER)

    data.zipWithIndex.filter(row_i => row_filter(row_i._2)).map( // filter rows
      _._1.zipWithIndex.filter(col_j => col_filter(col_j._2)).map(_._1)) // filter columns within each row

    // Layout

    e.config[Layout](MATRIX_LAYOUT) match {
      case _:Rows => data
      case _:Columns => data.transpose // @todo lazy transpose?
      case _ => throw new UnsupportedOperationException("Matrix Layout not 'Rows' or 'Columns'.")
    }

  }
}

object SeqOfRows {

  /** Parses matrix of doubles from json string.
    *  Shortcut to ``apply(Json(jsonStr),parseDouble)´´. */
  def apply[R:Real](jsonStr:String):Matrix[R] = apply(parse(jsonStr))

  lazy val parser = Json.Parsers.arrOf(Vec.parser)

  /** Parses Json array of arrays into a {{{Seq[Seq[R]]}}} */
  def parse[R:Real](json:String):Seq[Seq[R]]= {

    val real = implicitly[Real[R]]

    Json(json, parser).toArr.values.map {
      _.toArr.values.map {
        j:Json => real.json(j.toNum)
      } toVector
    }.toVector
  }



}