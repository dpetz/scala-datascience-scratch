package ds.matrix

trait Matrix[R] {

  type SS = Seq[Seq[R]]

  def rows: SS
  def columns: SS

  def validateShape(data: SS): Unit = {
    /** All row vectors to have equal length */
    require(data.tail.forall(_.size == data.head.size),
      s"${data.head.size} elements expected: ${data.find(_.size != data.head.size).get}"
    )
  }
}

object Matrix {

  def apply[T](data:Seq[Seq[T]]):Matrix[T] = throw new UnsupportedOperationException
}
