package ds.matrix

import ds.expr._
import ds.func._
import ds.num._
import ds.vec.Vec

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R](implicit real:Real[R]) extends Expr[Seq[Seq[R]]]{


  private val mf = Matrix.functions(real)
  private val sf = Scalar.functions(real)

  def eval(e:Engine):Seq[Seq[R]]

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

  def map(f:F1[R,R]):Matrix[R] = mf.map(f)(this)

  def transpose:Matrix[R] = Transpose(this)

  def aligned[R](other:Matrix[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:Matrix[R]):Matrix[R] = mf.plus(this,other)

  def +(x:Expr[R]):Matrix[R] = PlusReal(this,x)

  def *(other:Matrix[R]):Matrix[R] = Times(this,other)

  def *(x:Expr[R]):Matrix[R] = TimesReal(this,x)

}

object Matrix {

  def functions[R:Real]:Functions[R] = new Functions // @todo buffer

  class Functions[R:Real] {

    val real: Real[R] = implicitly[Real[R]]

    type SS = Seq[Seq[R]]

    private val sf = Scalar.functions(real)

    def zip(f:F2[R,R,R]):F2[SS,SS,SS]= F2("zip matrices",
      (e,m1,m2) => (e(m1) zip e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f.eval(e,xx._1, xx._2)))
    )

    def map(f:F1[R,R]):F1[SS,SS] =
      F1("map matrix", (e,m) => e(m).map(_ map (x => f.eval(e,x))))

    def plus:F2[SS,SS,SS] = zip(sf.plus)





  }

  /** Shortcut to ``[[Import(String)]]`` */
  def apply[R:Real](json:String,rows:Boolean=true):Matrix[R] =
    if (rows) Import[R](json) else throw UnsupportedOperationException

}