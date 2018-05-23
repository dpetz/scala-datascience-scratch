package math.lina

import io.Json

/**
  * Subclasses must implement at least [[rows]]
  */
trait Matrix[A] {
   
  /** Assert indices */
  
  private def assert[B](i:Int,j:Int)(f:(Int, Int)=>B): B = {
    if (i < 0 | i >= nRows) throw new IndexOutOfBoundsException(
        s"Row index not in [0,$nRows):$i")
    if (j < 0 | j >= nCols) throw new IndexOutOfBoundsException(
      s"Column index not in [0,$nCols}):$j")
    f(i,j)
  }

  /** Returns number of rows followed by number of columns. */
  def nRows:Int = rows.size
  def nCols:Int = rows(0).size
  def apply(i:Int,j:Int) = assert (i,j) (rows(_)(_))
  def cols:Seq[Seq[A]] = (0 to nCols-1).map { new ColView(this,_) }
  def rows:Seq[Seq[A]]
  
  override def toString = s"Matrix($nRows rows, $nCols columns)"
}
  
object Matrix {

  def apply(json:String):Matrix[Double]=RowsMatrix.fromJson(Json(json)).get
}
