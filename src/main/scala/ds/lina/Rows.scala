package ds.lina

case class Rows[A](matrix:Matrix[A]) extends  Seq[Row[A]] {

  def length = matrix.rows

  def apply(i:Int) = Row(matrix,i)

  /** Map this matrix as collection of rows */
  def iterator:Iterator[Row[A]]=(0 to length-1).map(Row(matrix,_)).iterator

}