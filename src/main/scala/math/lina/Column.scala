package math.lina

case class Column[A] (matrix:Matrix[A], index:Int) extends Seq[A] {
  def length = matrix.rows
  def apply(i:Int) = matrix(i,index)
  def iterator:Iterator[A]=(0 to matrix.rows-1).map(matrix(_,index)).iterator
}