package ds.matrix

case class Shape(rows:Int, cols:Int) {
  def transpose = Shape(cols, rows)
}
