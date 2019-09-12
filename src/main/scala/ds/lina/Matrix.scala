package ds.lina

import ds.num.Real
import parser.Json
import ds.lina.Vec

import scala.util.{Failure, Success, Try}


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



  /** Implements [[Matrix]] as a vector (the rows) of vectors (the entries).  */
  case class VecOfRowVecs[A](data:Seq[Seq[A]]) extends Matrix[A] {

    /** All row vectors to have equal length */
    require (data.forall( _.size == columns),
      s"$columns elements expected: ${data.find(_.size != columns).get}"
    )

    def apply(i:Int,j:Int)=data(i)(j)
    def rows=data.size
    def columns=data(0).size

  }

  /** Copies [[Json]] array of number arrays into scala vector of vectors */
  def apply[A](json:Json,parse:Json=>A):Try[Matrix[A]]= {
    try {
        Success(VecOfRowVecs[A](
          json.toArr.get.values.map {  // rows
                _.toArr.get.values.map { // row
                  parse(_)      // values
                }.toVector
          }.toVector
        ))
    } catch {
        case e:Exception => Failure(e)
    }
  }

  def parseDouble(json:Json):Double = json.toNum.get.value

  /** Parses matrix of doubles from json string.
   *  Shortcut to ``apply(Json(jsonStr),parseDouble)´´. */
  def apply(jsonStr:String):Matrix[Double]=
    apply[Double](Json(jsonStr),parseDouble).get




  /** [[Matrix]] utility methods such as [[map]]. */
  implicit class Ops[A](matrix:Matrix[A]) {

    def all = Elements(matrix)

    def zip[B](other:Matrix[B]):Matrix[(A,B)]={
      require (matrix aligned other)
      (matrix all) zip (other all) align matrix.columns
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
        }} columnize other.columns
    }
  }





}

