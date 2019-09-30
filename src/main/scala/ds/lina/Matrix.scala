package ds.lina

import ds.expr.Engine.{Layout, Rows}
import ds.expr.{Engine, Expr}
import ds.num.Real
import ds.lina.Vec._
import ds.lina.Matrix._

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R:Real](val shape:Shape) extends Expr[SS[R]]{

  //** Overwrite to change default implementation to just return this (ie. no evaluation required)  */
  def apply(e:Engine):SS[R]
  
  def rows: Int = shape.rows

  def cols: Int = shape.cols
  
  override def equals(other:Any):Boolean =
    other.isInstanceOf[M[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + rows @todo Hashcode w/o engine?

  override def toString = s"Matrix(${rows} rows, ${cols}‚ cols)"

  def zip(other:M[R],f:(R,R)=>R):M[R] = new Elementwise(this,other)(f)

  def map(f:R=>R):M[R] = Map(this)(_ => f)

  def transpose:M[R] = Transpose(this)

  def aligned[R](other:M[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:M[R]):M[R] = Plus(this,other)

  def +(x:Expr[R]):M[R] = PlusReal(this,x)

  def *(other:M[R]):M[R] = Times(this,other)

  def *(x:Expr[R]):M[R] = TimesReal(this,x)

}

object Matrix {
  
  type M[R] = Matrix[R]
  type S[R] = Seq[R]
  type SS[R] = Seq[S[R]]

  case class Shape(rows:Int, cols:Int) {
    def transpose = Shape(cols, rows)
  }

  def apply[R:Real](json:String,rows:Boolean=true):Matrix[R] =
    if (rows) SeqOfRows[R](json) else throw UnsupportedOperationException

  /** Transpose matrix */
  private case class Transpose[R: Real](m: M[R]) extends M[R](m.shape.transpose) {
    def apply(e: Engine): SS[R] = e.update(e.config[Layout]("Layout"))(m)
  }



  /** Add matrices */
  case class Plus[R](m1: M[R], m2: M[R])(implicit r: Real[R])
    extends Elementwise(m1, m2)(r.plus)

  /** Map matrix entries. */
  case class Map[R: Real](m: M[R])(f: Engine => R => R) extends M[R](m.shape) {
    def apply(e: Engine): SS[R] = e(m) map (v => v map f(e))
  }

  case class PlusReal[R](override val m: M[R], x: Expr[R])(implicit r: Real[R])
    extends Map[R](m)(e => r.plus(_, e(x)))

  /** Multiply matrix with real */
  case class TimesReal[R](override val m: M[R], x: Expr[R])(implicit r: Real[R])
    extends Map[R](m)(e => r.times(_, e(x)))

  /** Multiply matrices */
  case class Times[R:Real](m1: M[R], m2: M[R])
    extends Matrix[R](Shape(m1.rows, m2.cols)) {

    def eval(e: Engine): SS[R] = {

      require(m1.shape.transpose == m2.shape,
        s"Cannot multiply $m1 and $m2: Shapes do not fit.")

      // in var names assume m1 has rows layout. Columns case symmetric
      val m2_cols = e.update(Rows())(m2)
      e(m1) map (m1_row => m2_cols map (m2_col => e(m1_row dot m2_col))) // @todo support filters

    }
  }
}