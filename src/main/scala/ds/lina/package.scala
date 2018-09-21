package ds
import ds.algebra._
// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet


/**
 * Implicit class declaration for inline methods [[Seq]]
 * (representing ''vectors'') and [[Matrix]]
 */
package object lina {

  type Vec = Seq[Double]


implicit val vecGroup = new Group[Vec] {
  val zero = Nil
  
  def plus(l:Vec,r:Vec):Vec =
    if (l==zero) r
    else if (r==zero) l
    else (l zip r) map { case (x,y) => x+y }
  
  def negate(v:Vec):Vec =
    if (v==zero) v else v.map { -_ }

  def minus(l:Vec, r:Vec):Vec =
    if (r==zero) l
    else if (l==zero) r.map { -_ }
    else (l zip r) map { case (x,y) => x-y }
}


  implicit class SequenceOps[A](seq:Seq[A]) {

    /** View sequence as [[Matrix]] with specified number of columns */
    def columnize(cols:Int):Matrix[A]=Columnized(seq,cols)


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

  implicit class SequenceMath[A:Field](seq:Seq[A]) {

    // context bound
    val algebra = implicitly[Field[A]]

    // sum already used by [[Seq]]
    def total = seq.fold(algebra.zero)(algebra.plus)

    /** Dot product **/
    def dot(other:Seq[A]):A = (seq * other) total

    /** zips and applies binary operation */
    private def op(other: Seq[A], f: (A,A)=>A) = {
      require (seq.size == other.size)
      (seq zip other) map (x => f(x._1, x._2))
    }

    /** Add elementwise */
    def +(other: Seq[A]) = op(other, algebra.plus)

    /** Substract elementwise */
    def -(other: Seq[A]) = op(other, algebra.minus)

    /** Add constant */
    def +(x: A) = seq map (algebra.plus(_,x))

    /* Multiply elementwise */
    def *(w: Seq[A]) = op(w, algebra.times)
  }

  /**
   * [[MatrixOps]] that require  you can calculate with the entries
   * via a `Field[A]]`
   */
  implicit class MatrixMath[A:Field](matrix:Matrix[A])  {

    // context bound
    val algebra = implicitly[Field[A]]

    def +(other:Matrix[A]) =
      (matrix zip other) map algebra.plus
    
    def test(other:Matrix[A]) =
      (matrix zip other) map algebra.plus

    def *(x:A) =
      matrix map { algebra.times(x,_) }

    def *(other:Matrix[A]):Matrix[A] = {
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