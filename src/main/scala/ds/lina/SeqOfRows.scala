package ds.lina

import ds.num.Real
import parser.Json

import scala.util.{Failure, Success, Try}

/** Implements [[Matrix]] as a vector (the rows) of vectors (the entries).  */
case class SeqOfRows[A](data:Seq[Seq[A]]) extends Matrix[A] {

  /** All row vectors to have equal length */
  require (data.forall( _.size == columns),
    s"$columns elements expected: ${data.find(_.size != columns).get}"
  )

  def apply(i:Int,j:Int)=data(i)(j)
  def rows=data.size
  def columns=data(0).size

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
        j:Json => real.json(j.toNum.raw)
      } toVector
    }.toVector
  }



}