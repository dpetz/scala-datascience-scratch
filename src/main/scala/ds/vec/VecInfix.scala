package ds.vec
import parser.{Json, Parser}
import ds.expr.Func.{F1, F2, F3}
import ds.expr._
import ds.num.Real

import scala.Function.tupled



/** Wraps ``Expr`` evaluating to ``Seq[R]`` for vector infix operations
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
class VecInfix[R:Real](val expr:Expr[Seq[R]]) extends Expr[Seq[R]] {

  val real: Real[R] = implicitly[Real[R]]

  private val Elementwise = real.vec.Elementwise


  def apply(e:Engine):Seq[R] = expr(e)

  def size:Int = Size(this)

  def sum: Sum[R] = Sum(this)

  def map(f: F1[R,R]): VecInfix[R] = real.vec.Map(this, f)

  /** Dot product **/
  def dot(w: VecInfix[R]): Dot[R] = Dot(this, w)

  /** Calculates the p-norm */
  def norm(p: Expr[R]): Norm[R] = Norm(this, p)

  /** Add elementwise */
  def +(w: VecInfix[R]): VecInfix[R] = Elementwise(real.func.Plus, this, w)

  /** Multiply elementwise */
  def *(w: VecInfix[R]): VecInfix[R] = Elementwise(real.func.Times, this, w)

  /** Divide elementwise */
  def /(w: VecInfix[R]): VecInfix[R] = Elementwise(real.func.Div, this, w)

  /** Substract elementwise */
  def -(w: VecInfix[R]): VecInfix[R] = Elementwise(real.func.Minus, this, w)

  def unary_- : VecInfix[R] = Map()(this,real.negate _)




  /** Add constant */
  def +(x: Expr[R])(implicit r: Real[R]): VecInfix[R] = Map(this)( ds.num.Plus('v',x))

  /** Multiply constant */
  def *(x: Expr[R])(implicit r: Real[R]): VecInfix[R] = Map(this)(e => r.times(_,e(x)))


}

object VecInfix {

  type E[R] = Expr[R]



  /** Wraps ``Seq`` as [[Expr]] */


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
  def apply[R: Real](s: String)(implicit r: Real[R]): VecInfix[R] = seq2Vec(
    Json(s, parser).toArr.values.map {
      j: Json => r.json(j.toNum)
    })

  /** Parse sequence of doubles via [[Real.apply]] */
  def apply[R](doubles: Seq[Double])(implicit real: Real[R]): VecInfix[R] =
    doubles.map(real(_))

  implicit def seq2Vec[R: Real](s: Seq[R]): VecInfix[R] = new SeqVec[R](s)

  /** zips and applies binary operation */
  def each[R](v: Seq[R], w: Seq[R], f: (R, R) => R): Seq[R] = {
    require(v.size == w.size)
    (v zip w) map (x => f(x._1, x._2))
  }

}

