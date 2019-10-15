package ds.matrix

import ds.expr.Engine
import ds.matrix.Layout.{Columns, Rows}
import ds.num.Real
import ds.vec.Vec
import parser.Json

/**  */
case class Import[R](json:String, rowLayout:Boolean=true)(implicit real:Real[R]) extends Matrix[R] {

  val shape:Shape = Shape(
    if (rowLayout) parsed.size else parsed.head.size ,
    if (rowLayout) parsed.head.size else parsed.size,
    if (rowLayout) Rows() else Columns()
  )

  def apply(e:Engine):Seq[Seq[R]] = (new SeqOfSeq(parsed, rowLayout))(e)

  /** Parses Json array of arrays into a ``Seq[Seq[R`` */
  lazy val parsed:Seq[Seq[R]]= {

    val parser = Json.Parsers.arrOf(Vec.parser)

    Json(json, parser).toArr.values.map {
      _.toArr.values.map {
        j:Json => real.json(j.toNum)
      } toVector
    }.toVector
  }
}


