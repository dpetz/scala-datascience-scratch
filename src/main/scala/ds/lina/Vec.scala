package ds.lina

import ds.num.Real
import ds.num.Real.Infix
import scala.Function.tupled
import ds.lina.Matrix.Elem


object Vec {

  implicit class Ops[A](seq:Seq[A]) {

    /** View sequence as [[Matrix]] with specified number of columns */
    def align(cols:Int):Matrix[A]=Columnized(seq,cols)

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

  /** Infix operations to apply Real[R] operations elementwise to Seq[R] */
  implicit class Math[R:Real](v:Seq[R]) {

    // context bound
    val real = implicitly[Real[R]]

    // sum already used by [[Seq]]
    def total = v.fold(real.zero)(real.plus)

    /** Dot product **/
    def dot(other:Seq[R]):R = (v * other) total

    /** Calculates the p-norm */
    def norm(p:R):R = (v dot(v)) ** p

    /** zips and applies binary operation */
    private def each(other: Seq[R], f: (R,R)=>R) = {
      require (v.size == other.size)
      (v zip other) map (x => f(x._1, x._2))
    }

    /** Add elementwise */
    def +(other: Seq[R]) = each(other, real.plus)

    /** Substract elementwise */
    def -(other: Seq[R]) = each(other, real.minus)


    def unary_- = v map (real.negate(_))


    /* Multiply elementwise */
    def *(w: Seq[R]) = each(w, real.times)

    /* Divide elementwise */
    def /(w: Seq[R]) = each(w, real.div)



    /** Add constant */
    def +(x: R) = v map (real.plus(x,_))

    def *(x:R) = v map (real.times(x,_))
  }

}
