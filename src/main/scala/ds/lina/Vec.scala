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

  abstract class VecExpr[R](eval:Engine[R] => Vec[R]) extends E[Vec[R]]

   case class VecVec2Vec[R:Real] (v:Vec[R],w:Vec[R])(f:(Vec[R],Vec[R],Real[R]) => Vec[R])
      extends Vec[R](e => f(e(v),e(w),e.real))


  case class SeqVec[R:Real](s:Seq[R]) extends Vec[R](_ => s)


  case class Sum[R](v:Vec[R]) extends RealExpr[R] ( e =>
    e(v).fold(e.real.zero)(e.real.plus)
  )


  //class Vec2Vec[R](val v:Vec[R])(eval:Engine[R] => Vec[R] ) extends Vec[R]

  case class Map[R,S](v:Vec[R])(f:R=>S) extends Vec(v) {
    e => e(v).map(f)
  }

    /** Wraps ``Seq`` as [[Expr]]*/
   class Vec[R](val eval:Engine[R] => E[Seq[R]])(implicit real:Real[R]) extends E[Seq[R]] {

    /*--------------------------------------------------------------------*/
    /* Infix operations to apply Real[R] operations elementwise to Seq[R] */
    /*--------------------------------------------------------------------*/

      def sum:E[R] = Sum(this)

    def map[S:Real](f:R=>R) = Vec(toSeq.map(f))

    def size:Int = toSeq.size

    /** Dot product **/
    def dot(other:Vec[R]):R = new VecVec(this,other)({_ * _) sum})

    /** Calculates the p-norm */
    def norm(p:E[R]):E[R] = Norm(this,p)


    /** zips and applies binary operation */
    private def each(w: Vec[R], f: (R,R)=>R)(implicit e:Engine[R]): Vec[R] = Vec[R] {
      require (size == w.size)
      (e(this) zip e(w)) map (x => f(x._1, x._2))
    }

    /** Add elementwise */
    def +(w: Vec[R]): Vec[R] = VecVec2Vec[R](this,w)( (x,y,r) => x.each(y, r.plus) )

    /** Substract elementwise */
    def -(w: Vec[R]): Vec[R] = VecVec2Vec[R](this,w)( (x,y,r) => x.each(y, r.minus) )

    def unary_- : Vec[R] = map (real.negate(_))



    /** Multiply elementwise */
    def *(w: Vec[R]): Vec[R] = VecVec2Vec[R](this,w)( (x,y,r) => x.each(y, r.times) )

    /** Divide elementwise */
    def /(w: Vec[R]): Vec[R] = VecVec2Vec[R](this,w)( (x,y,r) => x.each(y, r.div) )

    /** Add constant */
    def +(x: E[R]): Vec[R] = map (real.plus(x,_))

    /** Multiply constant */
    def *(x:E[R]): Vec[R] = map (real.times(x,_))

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

