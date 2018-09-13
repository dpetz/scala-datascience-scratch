package ds.lina

case class Columns[A](matrix:Matrix[A]) extends  Seq[Column[A]] {

  def length = matrix.columns

  def apply(j:Int) = Column(matrix,j)

  /** Map this matrix as collection of rows */
  def iterator:Iterator[Column[A]]=(0 to length-1).map(Column(matrix,_)).iterator

}