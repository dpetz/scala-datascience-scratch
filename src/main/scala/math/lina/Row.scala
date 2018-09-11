package math.lina

case class Row[A] (matrix:Matrix[A], index:Int) extends Seq[A] {
  def length:Int=matrix.columns
  def apply(j:Int) = matrix(index,j)
  def iterator:Iterator[A]=(0 to matrix.columns-1).map(matrix(index,_)).iterator
}