package math.lina

case class Column[A] (j:Int, mat:Matrix[A]){
  def apply(i:Int) = mat(i,j)
  def elems[B](f:Elem[A]=>B):Seq[B]=(0 to mat.n-1).map(i => Elem(i,j,mat(i,j))).map(f)
}