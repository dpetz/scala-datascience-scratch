package ds.lina

import ds.num.Real
import ds.num.Real.RealInfix

import scala.Function.tupled
import parser.Json

/**
  *
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
  */
object Vec {

  type Vec[R] = Seq[R]

  case class Elem[A](x:A,i:Int)

  lazy val parser = Json.Parsers.arrOf(Json.Parsers.num)

  /** Parse from json string */
  def apply[R:Real](s:String) : Seq[R]  = {
    val real = implicitly[Real[R]]
    Json(s,parser).toArr.values.map {
      j:Json => real.json(j.toNum)
    }
  }

  /** Parse sequence of doubles via [[Real.apply]] */
  def apply[R](doubles:Seq[Double])(implicit real:Real[R]):Vec[R] =
    doubles.map(real(_))

  /** Infix operations for sequences that does not require a [[Rea]]l*/
  implicit class VecOps[A](seq:Seq[A]) {

    /** View sequence as [[Matrix]] with specified number of columns */
    def align(cols:Int):Matrix[A]=Columnized(seq,cols)

    /** Convert to Json string. */
    def json:String = seq.mkString("[", ",", "]")

    /** Zip with indices and map to [[Elem]] */
    def indexed:Seq[Elem[A]] = seq.zipWithIndex map tupled {(x,i) => Elem(x,i)}

    /** View sequence as [[Matrix]] with specified number of columns */
    private case class Columnized[A](seq:Seq[A],columns:Int) extends Matrix[A] {
      assert(seq.size % columns == 0)
      def rows = seq.size / columns
      def apply(i:Int, j:Int) = seq(i*rows+j)
    }

    /** Returns random slices of given size.
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
  implicit class VecMath[R](v:Seq[R])(implicit real:Real[R]) {

    type Vec = Seq[R]

    // sum already used by [[Seq]]
    def total = v.fold(real.zero)(real.plus)

    /** Dot product **/
    def dot(other:Vec):R = (v * other) total

    /** Calculates the p-norm */
    def norm(p:R):R = (v dot(v)) ** p

    /** zips and applies binary operation */
    private def each(other: Vec, f: (R,R)=>R): Vec = {
      require (v.size == other.size)
      (v zip other) map (x => f(x._1, x._2))
    }

    /** Add elementwise */
    def +(other: Vec): Vec = each(other, real.plus)

    /** Substract elementwise */
    def -(other: Vec): Vec = each(other, real.minus)

    def unary_- : Vec = v map (real.negate(_))

    /** Multiply elementwise */
    def *(w: Vec): Vec = each(w, real.times)

    /** Divide elementwise */
    def /(w: Vec): Vec = each(w, real.div)

    /** Add constant */
    def +(x: R): Vec = v map (real.plus(x,_))

    /** Multiply constant */
    def *(x:R): Vec = v map (real.times(x,_))
  }

}
