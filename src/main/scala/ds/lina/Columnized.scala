package ds.lina

/** View sequence as [[Matrix]] with specified number of columns */
private case class Columnized[A](seq: Seq[A], columns: Int) extends Matrix[A] {
  assert(seq.size % columns == 0)

  def rows = seq.size / columns

  def apply(i: Int, j: Int) = seq(i * rows + j)
}