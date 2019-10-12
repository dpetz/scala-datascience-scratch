package ds.matrix

// Query: https://github.com/dpetz/scratch/blob/7d15fd0c96ae4227037fae487118046f3c57d8d7/src/main/scala/ds/lina/Query.scala

sealed abstract class Orientation {
  /** Transpose */
  def t:Orientation
}

case class Rows() extends Orientation {
  def t = Columns()
}
case class Columns() extends Orientation {
  def t = Rows()
}
