package ds.matrix

import ds.func.Func.{F1, F2, F3}
import ds.expr._
import ds.func.Func
import ds.num.{Real, Scalar}
import ds.vec.Vec

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R](implicit real:Real[R]) extends Expr[Seq[Seq[R]]]{

  private val mf = Matrix.functions(real)
  private val sf = Scalar.functions(real)

  def shape:Shape

  //def eval(e:Engine):Seq[Seq[R]]

  /** Shortcut for ``shape.rows`` */
  def rows: Int = shape.rows

  /** Shortcut for ``shape.cols`` */
  def cols: Int = shape.cols
  
  override def equals(other:Any):Boolean =
    other.isInstanceOf[Matrix[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + rows @todo Hashcode w/o engine?

  override def toString = s"Matrix(${rows} rows, ${cols}‚ cols)"

  def zip(other:Matrix[R],f:F2[R,R,R]):Matrix[R] = mf.zip(f,this,other)

  def map(f:F1[R,R]):Matrix[R] = Map(this)(_ => f)

  def transpose:Matrix[R] = Transpose(this)

  def aligned[R](other:Matrix[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:Matrix[R]):Matrix[R] = mf.plus(this,other)

  def +(x:Expr[R]):Matrix[R] = PlusReal(this,x)

  def *(other:Matrix[R]):Matrix[R] = Times(this,other)

  def *(x:Expr[R]):Matrix[R] = TimesReal(this,x)

}

object Matrix {



  def functions[R](r:Real[R]):Functions[R] = new Functions(r)

  class Functions[R] (val real:Real[R]) {

    type SS = Seq[Seq[R]]

    private val sf = Scalar.functions(real)

    val zip: F3[(R, R) => R, SS, SS, SS] = Func("zip", (f,m1,m2) =>
      (m1 zip m2) map (vv => (vv._1 zip vv._2) map (xx => f(xx._1, xx._2)))
    )

    val map:F2[R=>R,SS,SS] = Func("map", (f,m) => m map (_ map f))

    val plus:F2[SS,SS,SS] = zip ! sf.plus





  }

  /** Shortcut to ``[[Import(String)]]`` */
  def apply[R:Real](json:String,rows:Boolean=true):Matrix[R] =
    if (rows) Import[R](json) else throw UnsupportedOperationException

}