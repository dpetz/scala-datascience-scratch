package ds.matrix


import ds.matrix.Query.{Filter, Layout}

case class Query(rows:Filter, cols:Filter, layout:Layout) {
  def transpose:Query = Query(cols,rows, layout.transpose)
  def layout(l:Layout)=Query(rows,cols,l)
  def turn=Query(rows,cols,layout.transpose)
}

object Query {

  trait Filter {
    def apply(i:Int):Boolean
  }

  case class All() {
    def apply(i:Int):Boolean = true
  }

  sealed abstract class Layout {
    def transpose:Layout
  }

  case class Rows() extends Layout {
    def transpose = Columns()
  }
  case class Columns() extends Layout {
    def transpose = Rows()
  }


}
