package ds.matrix

import ds.expr.Engine.Config


abstract class Filter(rows:Boolean = true) extends Config {
  val name:String = if (rows) Filter.ROWS else Filter.COLUMNS
  def apply(i:Int):Boolean
}

case class All(rows:Boolean) extends Filter(rows) {
  def apply(i:Int):Boolean = true
}

object Filter {

  val ROWS = "Row Filter"
  val COLUMNS = "Column Layout"

}
