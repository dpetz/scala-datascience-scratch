package ds.vec
import parser.{Json, Parser}
import ds.expr.Engine
import ds.expr.Func.{F1, F3}
import ds.expr._
import ds.vec.Vec._
import ds.num.Real

import scala.Function.tupled

/** ``Expr`` evaluating to ``Seq[R]``
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
class Vec[R:Real](val expr:Expr[Seq[R]]) extends Expr[Seq[R]] {

  val real: Real[R] = implicitly[Real[R]]

  def apply(e:Engine):Seq[R]

  def size:Int

  def sum: Sum[R] = Sum(this)

  def map(f: F1[R,R]): Map[R] = Map(this)(f)

  /** Dot product **/
  def dot(w: Vec[R]): Dot[R] = Dot(this, w)

  /** Calculates the p-norm */
  def norm(p: E[R]): Norm[R] = Norm(this, p)

  /** Add elementwise */
  def +(w: Vec[R]): Vec[R] = new Vec(elementwise(real.Plus, this, w))

  /** Multiply elementwise */
  def *(w: Vec[R]): Vec[R] = new Vec(elementwise(real.Times, this, w))

  /** Divide elementwise */
  def /(w: Vec[R]): Vec[R] = new Vec(elementwise(real.Div, this, w))

  /** Substract elementwise */
  def -(w: Vec[R]): Vec[R] = new Vec(elementwise(real.Minus, this, w))

  def unary_- : Vec[R] = ds.vec.Negate(this)

  /** Add constant */
  def +(x: E[R])(implicit r: Real[R]): Vec[R] = Map(this)( ds.num.Plus('v',x))

  /** Multiply constant */
  def *(x: E[R])(implicit r: Real[R]): Vec[R] = Map(this)( e => r.times(_,e(x)))

  def elementwise: F3[(R, R) => R, Seq[R], Seq[R], Seq[R]] = Func("Elementwise",
      (f,v,w) => ( v zip w) map (x => f(x._1, x._2))
    )

}

object Vec {

  type E[R] = Expr[R]

  def apply[R:Real](s: Seq[R]): Vec[R] = SeqVec(s)

  /** Wraps ``Seq`` as [[Expr]] */
  case class SeqVec[R: Real](s: Seq[R]) extends Vec[R] {
    def apply(e:Engine):Seq[R] = s
  }

  implicit class SeqOp[A](seq:Seq[A]) {

    /** View sequence as [[ds.matrix.Matrix]] with specified number of columns */
    //def align(cols: Int): Matrix[A] = Columnized(seq, cols)

    /** Convert to Json string. */
    def json: String = seq.mkString("[", ",", "]")

    /** Zip with indices and map to [[Elem]] */
    def indexed: Seq[Elem[A]] = seq.zipWithIndex map tupled { (x, i) => Elem(x, i) }

    case class Elem[A](x: A, i: Int)

    /** Returns random slices of given size.
      * Each element returned once except remainder which is ignored */
    def randomSlices(size: Int = 1): TraversableOnce[Seq[A]] = {
      import scala.util.Random
      val vShuffled = Random.shuffle(seq)
      for (b <- 0 until seq.length / size)
        yield vShuffled.slice(b * size, b * size + size)
    }

    val locale = java.util.Locale.US

    /** String interpolate vector elements */
    def format(ipol: String, start: String = "(", end: String = ")"): String = {
      (seq /:) (start) { (s:String, d:A) =>
        s + {
          if (s != start) "," else ""
        } + (ipol formatLocal(locale, d))
      } + end
    }
  }

  lazy val parser: Parser[Json.Arr] = Json.Parsers.arrOf(Json.Parsers.num)

  /** Parse from json string */
  def apply[R: Real](s: String)(implicit r: Real[R]): Vec[R] = seq2Vec(
    Json(s, parser).toArr.values.map {
      j: Json => r.json(j.toNum)
    })

  /** Parse sequence of doubles via [[Real.apply]] */
  def apply[R](doubles: Seq[Double])(implicit real: Real[R]): Vec[R] =
    doubles.map(real(_))

  implicit def seq2Vec[R: Real](s: Seq[R]): Vec[R] = new SeqVec[R](s)

  /** zips and applies binary operation */
  def each[R](v: Seq[R], w: Seq[R], f: (R, R) => R): Seq[R] = {
    require(v.size == w.size)
    (v zip w) map (x => f(x._1, x._2))
  }

}

