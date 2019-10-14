package ds.matrix

import ds.expr.Expr
import ds.num.Real

case class Shape(rows:Int, cols:Int, layout:Layout) {
  def transpose = Shape(cols, rows, layout.transpose)
  def apply[R:Real](e:Expr[Seq[Seq[R]]]):Matrix[R] = Matrix(e,this)
}
