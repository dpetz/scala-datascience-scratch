package ds.lina

import ds.expr.Expr
import ds.num.real._
import ds.lina.Vec._
import ds.num.real.Real
import ds.expr.Engine
import ds.lina.Matrix._



abstract class Row


/**
  * Minimal interface for a matrix.
  *
  * @see [[Columns]], [[Rows]], [[Elements]]
  */
abstract class Matrix[R:Real] extends Expr[Seq[Seq[R]]] {


  /** Evaluate to sequence of rows */
  def rows(e: Engine[R]): Seq[Seq[R]] //= Transposed(this).columns(e)

  /** Evaluate to sequence of columns */
  def columns(e: Engine[R]): Columns[R]

  abstract class Elementwise[R: Real](m1: Matrix[R], m2: Matrix[R])(f: (R, R) => R) extends Matrix {

  def rows(e: Engine[R]) = {
    e.rows(m1).zip(e.rows(m2).map{ case (row1:Seq[R], row2:Seq[R]) => row1.zip(row2).map(x => f(x._1,x._2)}
  }

}

  case class Plus[R](m1:Matrix[R],m2:Matrix[R](implicit r:Real[R]) extends Elementwise(m1,m2)(real.plus)

  def +(other:Matrix[R]):Matrix[R] = Plus(this,other)

  override def equals(other:Any) =
    other.isInstanceOf[Matrix[A]] && (hashCode == other.hashCode)

  override def hashCode = (this all).hashCode + rows

  override def toString = s"Matrix($rows rows, $columns‚ columns)"


  // context bound

  val real = implicitly[Real[R]]


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

    private case class Transposed[R:Real](m:Matrix[R]) extends Matrix[R] {
      def rows = matrix.columns
      def columns = matrix.rows
      def apply(i:Int,j:Int)=matrix(j,i)
    }
  }



  }











}

