package ds
import ds.algebra._
import ds.calc._

import Function.tupled
// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet


/**
 * Implicit class declaration for inline methods [[Seq]]
 * (representing ''vectors''), [[ds.lina.Elements]] or other matrix representations
 */
package object lina {




  /** Defines [[Group]] operations (+ and -) for a double vectors. */
implicit val vecGroup = new Group[Vec] {
  val zero = Nil
  
  def plus(l:Vec,r:Vec):Vec =
    if (l==zero) r
    else if (r==zero) l
    else (l zip r) map { case (x,y) => x+y }
  
  override def negate(v:Vec):Vec =
    if (v==zero) v else v.map { -_ }

  def minus(l:Vec, r:Vec):Vec =
    if (r==zero) l
    else if (l==zero) r.map { -_ }
    else (l zip r) map { case (x,y) => x-y }
}

  case class Elem[A](x:A,i:Int)

  implicit class SequenceOps[A](seq:Seq[A]) {

    /** View sequence as [[Matrix]] with specified number of columns */
    def columnize(cols:Int):Matrix[A]=Columnized(seq,cols)

    def json:String = seq.mkString("[", ",", "]")

    def indexed:Seq[Elem[A]] = seq.zipWithIndex map tupled {(x,i) => Elem(x,i)}


    /** View sequence as [[Matrix]] with specified number of columns */
    private case class Columnized[A](seq:Seq[A],columns:Int) extends Matrix[A] {
    	assert(seq.size % columns == 0)
    	def rows = seq.size / columns
    	def apply(i:Int, j:Int) = seq(i*rows+j)
    }

    /* Returns random slices of given size.
     * Each element returned once except remainder which is ignored */
    def randomSlices(size:Int=1):TraversableOnce[Seq[A]]={
      import scala.util.Random
      val vShuffled = Random.shuffle(seq)
      for (b <- 0 until seq.length / size)
        yield vShuffled.slice(b*size,b*size+size)
    }

    val locale = java.util.Locale.US

     /** String interpolate vector elements */
    def format(ipol:String,start:String="(",end:String=")"):String={
      (seq /:) (start) { (s,d) =>
        s +  { if (s != start)  "," else "" } + ( ipol formatLocal(locale, d) )
      } + end
    }

  }

  implicit class SequenceMath[A:RealMath](seq:Seq[A]) {

    // context bound
    val alg = implicitly[RealMath[A]]

    // sum already used by [[Seq]]
    def total = seq.fold(alg.zero)(alg.plus)

    /** Dot product **/
    def dot(other:Seq[A]):A = (seq * other) total

    /** zips and applies binary operation */
    private def each(other: Seq[A], f: (A,A)=>A) = {
      require (seq.size == other.size)
      (seq zip other) map (x => f(x._1, x._2))
    }

    /** Add elementwise */
    def +(other: Seq[A]) = each(other, alg.plus)

    /** Substract elementwise */
    def -(other: Seq[A]) = each(other, alg.minus)


    def unary_- = seq map (alg.negate(_))


    /* Multiply elementwise */
    def *(w: Seq[A]) = each(w, alg.times)

    /* Divide elementwise */
    def /(w: Seq[A]) = each(w, alg.div)



    /** Add constant */
    def +(x: A) = seq map (alg.plus(x,_))

    def *(x:A) = seq map (alg.times(x,_))
  }

  /**
   * [[MatrixOps]] that require  you can calculate with the entries
   * via a `Field[A]]`
   */
  implicit class MatrixMath[R:RealMath](matrix:Matrix[R])  {

    // context bound
    val math = implicitly[RealMath[R]]

    def +(other:Matrix[R]):Matrix[R] =
      (matrix zip other) map { case (x,y) => math.plus(x,y) }

    def *(x:R):Matrix[R] =
      matrix map { math.times(x,_) }

    def *(other:Matrix[R]):Matrix[R] = {
      require (matrix.transpose aligned other,
        s"Cannot multiply $matrix and $other: Shapes do not fit.")
      Rows(matrix).flatMap {
        r => Columns(other).map {
        c => r dot c
      }} columnize other.columns
    }
  }

  /** [[Matrix]] utility methods such as [[map]]. */
  implicit class MatrixOps[A](matrix:Matrix[A]) {

  	def zip[B](other:Matrix[B]):Matrix[(A,B)]={
  		require (matrix aligned other)
  		(matrix elements) zip (other elements) columnize matrix.columns
  	}

  	def elements = Elements(matrix)

  	def map[B](f:A=>B):Matrix[B] =
      (matrix elements) map(f) columnize (matrix.columns)

  	def transpose:Matrix[A] = Transposed(matrix)

    def aligned[B](other:Matrix[B]) =
      (matrix.rows == other.rows) && (matrix.columns == other.columns)

  	private case class Transposed[A](matrix:Matrix[A]) extends Matrix[A] {
  		def rows = matrix.columns
  		def columns = matrix.rows
  		def apply(i:Int,j:Int)=matrix(j,i)
  	}

  }


}