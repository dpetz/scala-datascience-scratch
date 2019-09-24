package ds.lina

import ds.expr.Engine
import ds.expr._
import ds.num.real._

import scala.Function.tupled
import parser.Json


/**
  *
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
  */
object vec {

  type E[R] = Expr[R]


  /** Wraps ``Seq`` as [[Expr]]*/
  case class SeqVec[R](s:Seq[R]) extends Vec[R](_ => s)


  case class Sum[R](v:Vec[R]) extends RealValued[R] (e =>
    e(v).fold(e.zero)(e.plus)
  )

  case class Size[R](v:Vec[R]) extends RealValued[R](e => e.real(e(v).size) )


  case class Map[R](v:Vec[R])(f:R=>R) extends Vec[R](
    e => e(v).map(f)
  )

  case class Dot[R](v:Vec[R],w:Vec[R]) extends Composed[R](
    _ => (v * w) sum
  )

    /** ``Expr`` evaluating to ``Seq[R]`` */
   class Vec[R](val eval:Engine[R] => Seq[R]) extends E[Seq[R]] {

    /*--------------------------------------------------------------------*/
    /* Infix operations to apply Real[R] operations elementwise to Seq[R] */
    /*--------------------------------------------------------------------*/

      def sum:E[R] = Sum(this)

      def map(f:R=>R):Vec[R] = Map(this)(f)

      def size= Size(this)

      /** Dot product **/
    def dot(w:Vec[R]):Expr[R] = Dot(this,w)

    /** Calculates the p-norm */
    def norm(p:E[R]):E[R] = Norm(this,p)

    /** Add elementwise */
    def +(w: Vec[R]): Vec[R] = new Vec[R]( e => Vec.each(e(this), e(w), e.plus))

    /** Substract elementwise */
    def -(w: Vec[R]): Vec[R] = new Vec[R]( e => Vec.each(e(this), e(w), e.minus))

    def unary_- : Vec[R] = map (e.negate(_))


    /** Multiply elementwise */
    def *(w: Vec[R]): Vec[R] = Vec.Times(this,w)

    /** Divide elementwise */
    def /(w: Vec[R]): Vec[R] = new Vec[R]( e => Vec.each(e(this), e(w), real.div))

    /** Add constant */
    def +(x: E[R]): Vec[R] = map (real.plus(x,_))

    /** Multiply constant */
    def *(x:E[R]): Vec[R] = map (real.times(x,_))

  }

object Vec {
  /** zips and applies binary operation */
  def each[R](v:Seq[R], w: Seq[R], f: (R,R)=>R): Seq[R] = {
    require (v.size == w.size)
    (v zip w) map (x => f(x._1, x._2))
  }

  case class Times[R](v:Vec[R], w: Vec[R]) extends Vec[R]( e => Vec.each(e(v), e(w), e.times))


}

  implicit def seq2Vec[R](s:Seq[R]) = new Vec[R](s)


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


}

