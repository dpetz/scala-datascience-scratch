package ds.matrix

import ds.expr.{Engine, Expr}
import ds.num.Real

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R:Real] extends Expr[Seq[Seq[R]]]{

  def shape:Shape

  def apply(e:Engine):Seq[Seq[R]]

  /** Shortcut for ``shape.rows`` */
  def rows: Int = shape.rows

  /** Shortcut for ``shape.cols`` */
  def cols: Int = shape.cols
  
  override def equals(other:Any):Boolean =
    other.isInstanceOf[Matrix[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + rows @todo Hashcode w/o engine?

  override def toString = s"Matrix(${rows} rows, ${cols}‚ cols)"

  def zip(other:Matrix[R],f:(R,R)=>R):Matrix[R] = new Elementwise(this,other)(f)

  def map(f:R=>R):Matrix[R] = Map(this)(_ => f)

  def transpose:Matrix[R] = Transpose(this)

  def aligned[R](other:Matrix[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:Matrix[R]):Matrix[R] = Plus(this,other)

  def +(x:Expr[R]):Matrix[R] = PlusReal(this,x)

  def *(other:Matrix[R]):Matrix[R] = Times(this,other)

  def *(x:Expr[R]):Matrix[R] = TimesReal(this,x)

}

object Matrix {

  /** Shortcut to ``[[Import(String)]]`` */
  def apply[R:Real](json:String,rows:Boolean=true):Matrix[R] =
    if (rows) Import[R](json) else throw UnsupportedOperationException

}