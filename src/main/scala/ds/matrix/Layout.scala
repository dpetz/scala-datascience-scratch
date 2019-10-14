package ds.matrix

import ds.expr.Engine.Config


sealed abstract class Layout() extends Config {
  val name:String = Layout.NAME
  def transpose:Layout
}

object Layout {

  val NAME = "Matrix Layout"

  case class Rows() extends Layout {
    def transpose = Columns()
  }
  case class Columns() extends Layout {
    def transpose = Rows()
  }

}
