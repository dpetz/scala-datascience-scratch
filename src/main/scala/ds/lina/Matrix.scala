package ds.lina

import ds.expr.{Engine, Expr, Really}
import ds.num.real._
import ds.lina.Vec._
import ds.num.real.Real
import ds.lina.Matrix._
import ds.lina.Query._










/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  *
  * @see [[ColumnsOld]], [[RowsOld]], [[Elements]]
  */
abstract class Matrix[R:Real](val shape:Shape) extends Expr[SS[R]]{

  /** Gets entry by index */
  //def apply(i: Int, j: Int)(implicit e:Engine[R]): R

  //** Overwrite to change default implementation to just return this (ie. no evaluation required)  */
  def eval(e:Engine[R], q:Query):SS[R]

  override def equals(other:Any) =
    other.isInstanceOf[M[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + shape.rows @todo Hascode w/o engine?

  override def toString = s"Matrix(${shape.rows} rows, ${shape.cols}‚ columns)"

  def elementwise(other:M[R],f:(R,R)=>R):M[R] = new Elementwise(this,other)(f)

  def map(f:R=>R):M[R] = Map(this)(_ => f)

  def transpose:M[R] = Transpose(this)

  def aligned[R](other:M[R]):Boolean =
    (shape.rows == other.shape.rows) && (shape.cols == other.shape.cols)

  def +(other:M[R]):M[R] = Plus(this,other)

  def +(x:Really[R]):M[R] = PlusReal(this,x)


  def *(other:M[R]):M[R] = Times(this,other)

  def *(x:Really[R]):M[R] = TimesReal(this,x)
  
}

object Matrix {
  
  type M[R] = Matrix[R]
  type S[R] = Seq[R]
  type SS[R] = Seq[S[R]]

  private case class Transpose[R: Real](m: M[R]) extends M[R](m.shape.transpose) {
    def eval(e: Engine[R], q: Query): SS[R] = e(m, q.transpose)
  }

  class Elementwise[R: Real](m1: M[R], m2: M[R])(f: (R, R) => R) extends M[R](m1.shape) {
    def eval(e: Engine[R], q: Query): SS[R] =
      (e(m1, q) zip (e(m2, q)) map (vv => (vv._1 zip vv._2) map (xx => f(xx._1, xx._2))))
  }

  case class Plus[R](m1: M[R], m2: M[R])(implicit r: Real[R])
    extends Elementwise(m1, m2)(r.plus)

  case class Map[R: Real](m: M[R])(f: Engine[R] => R => R) extends M[R](m.shape) {
    def eval(e: Engine[R], q: Query): SS[R] = e(m, q) map (v => v map f(e))
  }

  case class PlusReal[R](override val m: M[R], x: Really[R])(implicit r: Real[R])
    extends Map[R](m)(e => r.plus(_, e(x)))

  case class TimesReal[R](override val m: M[R], x: Really[R])(implicit r: Real[R])
    extends Map[R](m)(e => r.times(_, e(x)))


  case class Times[R](m1: M[R], m2: M[R])(implicit r: Real[R])
    extends Matrix(Shape(m1.shape.rows, m2.shape.cols)) {

    def eval(e: Engine[R], q: Query): SS[R] = {

      require(m1.shape.transpose == m2.shape,
        s"Cannot multiply $m1 and $m2: Shapes do not fit.")

      // assume m1 has rows layout. other case symmetric
      val m2_cols = e(m2, q.turn)
      e(m1, q) map (m1_row => m2_cols map (m2_col => e(m1_row dot m2_col)))

    }
  }
}