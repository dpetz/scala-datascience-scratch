package ds.lina

import ds.expr.{Engine, Expr, Really}
import ds.num.real._
import ds.lina.Vec._
import ds.num.real.Real
import ds.lina.Matrix._



abstract class Row


/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  *
  * @see [[Columns]], [[Rows]], [[Elements]]
  */
abstract class Matrix[R:Real](val size:(Int,Int)) extends Expr[Matrix[R]]{

  /** Gets entry by index */
  def apply(i: Int, j: Int)(implicit e:Engine[R]): R

  //** Overwrite to change default implementation to just return this (ie. no evaluation required)  */
  def eval(e:Engine[R]):Matrix[R]

  def rows:Int = size._1
  val columns:Int = size._2


  override def equals(other:Any) =
    other.isInstanceOf[Matrix[R]] && (hashCode == other.hashCode)

  override def hashCode = (this all).hashCode + rows

  override def toString = s"Matrix($rows rows, $columns‚ columns)"

  def all:Seq[R] = Elements(this)

  def zip(other:Matrix[R],f:(R,R)=>R):Matrix[R]={
    require (this aligned other)
    all zip (other all) map (x => f(x._1,x._2)) align columns
  }

  def map[B](f:R=>R):Matrix[R] =
    all map f align columns

  def transpose:Matrix[R] = Transposed(this)

  def aligned[R](other:Matrix[R]):Boolean =
    (rows == other.rows) && (columns == other.columns)

  def +(other:Matrix[R]):Matrix[R] = Plus(this,other)


}


object Matrix {


  type M[R] = Matrix[R]

  private case class Transposed[R:Real](m:Matrix[R]) extends Matrix[R](m.size) {

    def eval(e:Engine[R]):Matrix[R] = this
    def apply(i:Int,j:Int):R = m(j,i)
  }


  abstract class Zip[R: Real](m1: Matrix[R], m2: Matrix[R])(f: (R, R) => R)
    extends Matrix[R](m1.size) {
     override def eval(e:Engine[R]):Matrix[R] = e(m1) zip (e(m2), f)
  }

  case class Plus[R](m1:Matrix[R],m2:Matrix[R])(implicit r:Real[R])
    extends Zip(m1,m2)(r.plus)

  case class Map[R](m:Matrix[R])(f:R=>R)(implicit r:Real[R])
    extends Matrix[R](m.rows, m.columns)(e => e(m).map(f))

  case class PlusReal[R](m:Matrix[R],x:Really[R])(implicit r:Real[R])
    extends Map(m)(r.plus(_,e(x)))



  implicit def matrix2Expr[R:Real](m:Matrix[R]) = new Matrix[R](_ => m)



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

  def apply[R:Real](jsonRows:String) = SeqOfRows(jsonRows)

  /** [[Matrix]] utility methods such as [[map]]. */
  implicit class Ops[A](matrix:Matrix[A]) {





  }











}

