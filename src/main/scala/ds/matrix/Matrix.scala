package ds.matrix

import ds.expr._
import ds.func._
import ds.num._

/**
  * Minimal interface for a matrix
  * Evaluates to itself in the sense that all operations that might have stacked up are performed.
  */
abstract class Matrix[R](implicit real:Real[R]) extends Expr[Seq[Seq[R]]]{


  private val mf = Matrix.functions(real)
  private val sf = Scalar.functions(real)

  def eval(e:Engine):Seq[Seq[R]]

  val shape:Shape

  //def eval(e:Engine):Seq[Seq[R]]

  /** Shortcut for ``shape.rows`` */
  def rows: Int = shape.rows

  /** Shortcut for ``shape.cols`` */
  def cols: Int = shape.cols
  
  override def equals(other:Any):Boolean =
    other.isInstanceOf[Matrix[R]] && (hashCode == other.hashCode)

  // override def hashCode = (this all).hashCode + rows @todo Hashcode w/o engine?

  override def toString = s"Matrix($rows rows, $cols‚ cols)"

  def zip(other:Matrix[R],f:F2[R,R,R]):Matrix[R] = shape( mf.zip(f)(this,other) )

  def map(f:F1[R,R]):Matrix[R] = shape ( mf.map(f)(this) )

  def transpose:Matrix[R] = Transpose(this)

  def aligned(other:Matrix[R]):Boolean =
    (rows == other.rows) && (cols == other.cols)

  def +(other:Matrix[R]):Matrix[R] = shape( mf.plus(this,other) )

  def +(x:Expr[R]):Matrix[R] = shape( mf.plus(x)(this) )

  def *(other:Matrix[R]):Matrix[R] = shape( mf.times(this,other) )

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

    def zip(f:F2[R,R,R]):F2[SS[R],SS[R],SS[R]]= F2("zip matrices",
      (e,m1,m2) => (e(m1) zip e(m2)) map (vv => (vv._1 zip vv._2) map (xx => f.eval(e,xx._1, xx._2)))
    )

    def map(f:F1[R,R]):F1[SS[R],SS[R]] =
      F1("map matrix", (e,m) => e(m).map(_ map (x => f.eval(e,x))))

    def plus:F2[SS[R],SS[R],SS[R]] = zip(sf.plus)

    def plus(x:Expr[R]):F1[SS[R],SS[R]] = map(sf.plus ! x)

    /** Multiply matrices */
    def times:F2[SS[R],SS[R],SS[R]] = F2[SS[R],SS[R],SS[R]]= F2("times matrix",  (e,m1,m2) => {

      //val shape:Shape = Shape(m1.rows, m2.cols)

        require(m1.shape.transpose == m2.shape,
          s"Cannot multiply $m1 and $m2: Shapes do not fit.")

        // in var names assume m1 has rows layout. Columns case symmetric
        val m2_cols = e.update(Columns())(m2)
        e(m1) map (m1_row => m2_cols map (m2_col => e(m1_row dot m2_col))) // @todo support filters
    }


    def times(x:Expr[R]):F1[SS[R],SS[R]] = map(sf.times ! x)




  }


}