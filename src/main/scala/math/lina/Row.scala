package math.lina

case class Row[A] (i:Int, mat:Matrix[A]){
  def apply(j:Int) = mat(i,j)
  def elems[B](f:Elem[A]=>B):Seq[B]=(0 to mat.m-1).map(j => Elem(i,j,mat(i,j))).map(f)
}