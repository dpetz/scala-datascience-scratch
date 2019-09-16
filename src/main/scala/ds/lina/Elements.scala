package ds.lina

/** View [[Matrix]] as [[Seq]] off all elements */
case class Elements[A](matrix:Matrix[A],byRow:Boolean=true) extends Seq[A] {

  def length = matrix.rows * matrix.columns

  def apply(i:Int) =
  	if (byRow) matrix(i / matrix.rows, i % matrix.rows)
  	else matrix(i % matrix.columns, i / matrix.columns)
  
  def iterator:Iterator[A]=(0 to length-1).map(apply).iterator

}
