package ds.lina

import ds.num.Real
import ds.lina.VectorUtil.{VecOps,VecMath}



/**
  * Minimal interface for a matrix.
  *
  * @see [[Columns]], [[Rows]], [[Elements]]
  */
trait Matrix[A] {

  /** Number of rows */
  def rows:Int

  /** Number of columns */
  def columns:Int

  /** Gets entry by index*/
  def apply(i:Int,j:Int):A

  override def equals(other:Any) =
    other.isInstanceOf[Matrix[A]] && (hashCode == other.hashCode)

  override def hashCode = (this all).hashCode + rows

  override def toString = s"Matrix($rows rows, $columns‚ columns)"

}

object Matrix {

  def apply[R:Real](jsonRows:String) = SeqOfRows(jsonRows)

  /** [[Matrix]] utility methods such as [[map]]. */
  implicit class Ops[A](matrix:Matrix[A]) {

    /** Sequence off all elements.
      * @see Elements */
    def all:Seq[A] = Elements(matrix)

    def zip[B](other:Matrix[B]):Matrix[(A,B)]={
      require (matrix aligned other)
      (matrix all) zip (other all) align (matrix.columns)
    }

    def map[B](f:A=>B):Matrix[B] =
      (matrix all) map(f) align (matrix.columns)

    def transpose:Matrix[A] = Transposed(matrix)

    def aligned[B](other:Matrix[B]) =
      (matrix.rows == other.rows) && (matrix.columns == other.columns)

    private case class Transposed[A](matrix:Matrix[A]) extends Matrix[A] {
      def rows = matrix.columns
      def columns = matrix.rows
      def apply(i:Int,j:Int)=matrix(j,i)
    }

  }

  /**
    * [[MatrixOps]] that require  you can calculate with the entries
    * via a `Field[A]]`
    */
  implicit class Math[R:Real](matrix:Matrix[R])  {

    // context bound

    val real = implicitly[Real[R]]

    def +(other:Matrix[R]):Matrix[R] =
      (matrix zip other) map { case (x,y) => real.plus(x,y) }

    def *(x:R):Matrix[R] =
      matrix map { real.times(x,_) }

    def *(other:Matrix[R]):Matrix[R] = {
      require (matrix.transpose aligned other,
        s"Cannot multiply $matrix and $other: Shapes do not fit.")
      Rows(matrix).flatMap {
        r => Columns(other).map {
          c => r dot c
        }} align other.columns
    }
  }





}

