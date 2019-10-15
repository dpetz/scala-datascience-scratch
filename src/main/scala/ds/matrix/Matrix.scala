package ds.matrix

import ds.expr._
import ds.func._
import ds.matrix.Layout.Columns
import ds.num._
import ds.vec._

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R](implicit real:Real[R]) extends Closed[Seq[Seq[R]]] {

  private val mf = Matrix.functions(real)

  val shape:Shape

  /** Shortcut for ``shape.rows`` */
  def rows: Int = shape.rows

  /** Shortcut for ``shape.cols`` */
  def cols: Int = shape.cols
  
  override def equals(other:Any):Boolean =
    other.isInstanceOf[Matrix[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + rows @todo Hashcode w/o engine?

  override def toString = s"Matrix($rows rows, $cols‚ cols)"

  def zip(other:Matrix[R],f:F2[Expr[R],Expr[R],R]):Matrix[R] = shape( mf.zip(f)(this,other) )

  def map(f:F1[Expr[R],R]):Matrix[R] = shape ( mf.map(f)(this) )

  def transpose:Matrix[R] = shape.transpose( mf.transpose(this) )

  def aligned(other:Matrix[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:Matrix[R]):Matrix[R] = shape( mf.plus(this,other) )

  def +(x:Expr[R]):Matrix[R] = shape( mf.plus(x)(this) )

  def *(other:Matrix[R]):Matrix[R] = Matrix( mf.times(this,other), Shape(rows, other.cols, shape.layout) )

  def *(x:Expr[R]):Matrix[R] = shape( mf.times(x)(this) )

}

object Matrix {

  type SS[R] = Seq[Seq[R]]

  def apply[R:Real](e:Expr[SS[R]],s:Shape):Matrix[R] = {
    class Expr2Matrix(override val express:Expr[SS[R]], override val shape:Shape)
      extends Matrix[R] with Expressible[SS[R]]
    new Expr2Matrix(e,s)
  }

  /** Shortcut to ``[[Import(String)]]`` */
  def apply[R:Real](json:String,rows:Boolean=true):Matrix[R] =
    if (rows) Import[R](json) else throw UnsupportedOperationException


  def functions[R:Real]:Functions[R] = new Functions // @todo buffer

  class Functions[R:Real] {

    val real: Real[R] = implicitly[Real[R]]

    private val sf = Scalar.functions(real)

    def zip(f:F2[Expr[R],Expr[R],R]):F2[Matrix[R],Matrix[R],SS[R]]= F2("zip matrices",
      (e,m1,m2) => (e(m1) zip e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f.eval(e,xx._1, xx._2)))
    )

    def map(f:F1[Expr[R],R]):F1[Matrix[R],SS[R]] =
      F1("map matrix", (e,m) => e(m).map(_ map (x => f.eval(e,x))))

    def transpose(m:Matrix[R]):F1[Matrix[R],SS[R]] = F1("transpose", (e,m) =>
      e.update(e.config[Layout](Layout.NAME).transpose)(m)
    )

    def plus:F2[Matrix[R],Matrix[R],SS[R]] = zip(sf.plus)

    def plus(x:Expr[R]):F1[Matrix[R],SS[R]] = map(sf.plus ! x)

    /** Multiply matrices */
    def times:F2[Matrix[R],Matrix[R],SS[R]] = F2[Matrix[R],Matrix[R],SS[R]]("times matrix",  (e,m1,m2) => {

        require(m1.shape.transpose == m2.shape,
          s"Cannot multiply $m1 and $m2: Shapes do not fit.")

        // in var names assume m1 has rows layout. Columns case symmetric
        val m2_cols = e.update(Columns())(m2)
        e(m1) map (m1_row => m2_cols map (m2_col => e(m1_row dot m2_col))) // @todo support filters
    })

    def times(x:Expr[R]):F1[Matrix[R],SS[R]] = map(sf.times ! x)

  }


}