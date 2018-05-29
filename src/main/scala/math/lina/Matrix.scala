package math.lina

import io.Json

import scala.collection.{Map,AbstractSeq,AbstractMap}

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
  def cols:Seq[Seq[A]] = (0 to nCols-1).map { new Matrix.Column(this,_) }
  def rows:Seq[Seq[A]]
  def toMap:Map[(Int,Int),A] = new Matrix.AsMap[A](this)

  
  override def toString = s"Matrix($nRows rows, $nCols columns)"
}
  
object Matrix {

  type Index =(Int, Int)

  /** Shortcut to [[RowsMatrix.fromJson]]. */
  def apply(json:String):Matrix[Double]=RowsMatrix.fromJson(Json(json)).get

  /** Wraps column as [[Seq]] */
  class Column[A](val m:Matrix[A], val j:Int) extends AbstractSeq[A] {
    def apply(i:Int):A = m(i,j)
    def iterator:Iterator[A] = (0 to length-1).map(m(_,j)).iterator
    def length:Int=m.nRows
  }

  /** Wraps matrix as [[Map]] */
  class AsMap[A](mat:Matrix[A]) extends AbstractMap[Index,A] {

    def get(idx:Index):Option[A] = 
      try { Some(mat(idx._1, idx._2)) }
      catch { case e:Exception => None }
    
    def iterator = mat.rows.zipWithIndex.flatMap {
       case (row,i) => row.zipWithIndex.map {
          case (x,j) => ((i,j),x) }}.iterator

    /** Unsupported because result not rectangular. */
    def +[B >: A](kv:(Index,B)):Map[Index,B] = throw new UnsupportedOperationException

    /** Unsupported because result not rectangular. */
    def -(k:Index) = throw new UnsupportedOperationException

  }
}
