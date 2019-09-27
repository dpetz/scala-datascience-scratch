package ds.lina
import parser.Json
import ds.expr.Engine
import ds.expr._
import ds.lina.Vec._
import ds.num.real._
import scala.Function.tupled

/** ``Expr`` evaluating to ``Seq[R]``
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
class Vec[R:Real](val eval:Engine[R] => Seq[R]) extends Expr[R,Seq[R]] {

  def sum: Sum[R] = Sum(this)

  def map(f: R => R): Map[R] = Map(this)(f)

  /** Dot product **/
  def dot(w: Vec[R]): Dot[R] = Dot(this, w)

  /** Calculates the p-norm */
  def norm(p: E[R]): Norm[R] = Norm(this, p)

  /** Add elementwise */
  def +(w: Vec[R]): Vec[R] = Plus(this, w)

  /** Multiply elementwise */
  def *(w: Vec[R]): Times[R] = Times(this, w)

  /** Divide elementwise */
  def /(w: Vec[R]): Div[R] = Div(this, w)

  /** Substract elementwise */
  def -(w: Vec[R]): Minus[R] = Minus(this, w)

  def unary_- : Vec[R] = Negate(this)

  /** Add constant */
  def +(x: Really[R])(implicit r: Real[R]): Vec[R] = Constant(this, x)(r.plus)

  /** Multiply constant */
  def *(x: Really[R])(implicit r: Real[R]): Vec[R] = Constant(this, x)(r.times)
}

object Vec {

  implicit class SeqOp[A](seq:Seq[A]) {

    /** View sequence as [[Matrix]] with specified number of columns */
    //def align(cols: Int): Matrix[A] = Columnized(seq, cols)

    /** Convert to Json string. */
    def json: String = seq.mkString("[", ",", "]")

    /** Zip with indices and map to [[Elem]] */
    def indexed: Seq[Elem[A]] = seq.zipWithIndex map tupled { (x, i) => Elem(x, i) }


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
      (seq /:) (start) { (s, d) =>
        s + {
          if (s != start) "," else ""
        } + (ipol formatLocal(locale, d))
      } + end
    }
  }


  lazy val parser = Json.Parsers.arrOf(Json.Parsers.num)

  /** Parse from json string */
  def apply[R: Real](s: String)(implicit r: Real[R]): Vec[R] = seq2Vec(
    Json(s, parser).toArr.values.map {
      j: Json => r.json(j.toNum)
    })

  /** Parse sequence of doubles via [[Real.apply]] */
  def apply[R](doubles: Seq[Double])(implicit real: Real[R]): Vec[R] =
    doubles.map(real(_))

  implicit def seq2Vec[R: Real](s: Seq[R]): Vec[R] = new Vec[R](_ => s)

  case class Elem[A](x: A, i: Int)

  /** zips and applies binary operation */
  def each[R](v: Seq[R], w: Seq[R], f: (R, R) => R): Seq[R] = {
    require(v.size == w.size)
    (v zip w) map (x => f(x._1, x._2))
  }

  /** Wraps ``Seq`` as [[Expr]] */
  case class SeqVec[R: Real](s: Seq[R]) extends Vec[R](_ => s)

  case class Sum[R](v: Vec[R])(implicit r: Real[R]) extends Really[R](
    e => e(v).fold(r.zero)(r.plus)
  )

  case class Size[R](v: Vec[R])(implicit r: Real[R]) extends Really[R](
    e => r(e(v).size)
  )


  case class Map[R: Real](v: Vec[R])(f: R => R) extends Vec[R](
    e => e(v).map(f)
  )

  case class Dot[R](v: Vec[R], w: Vec[R]) extends Composed[R](
    _ => (v * w) sum
  )

  case class Negate[R](v: Vec[R])(implicit r: Real[R]) extends Vec[R](e => e(v).map(r.negate))

  class Elementwise[R](v: Vec[R], w: Vec[R])(f: (R, R) => R)(implicit r: Real[R])
    extends Vec[R](e => each(e(v), e(w), f))


  case class Times[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
    extends Elementwise[R](v, w)(r.times)

  case class Plus[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
    extends Elementwise[R](v, w)(r.plus)

  case class Minus[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
    extends Elementwise[R](v, w)(r.minus)

  case class Div[R](v: Vec[R], w: Vec[R])(implicit r: Real[R])
    extends Elementwise[R](v, w)(r.div)

  case class Norm[R: Real](v: Vec[R], p: Expr[R]) extends Composed[R](e =>
    e(p) match {
      case 1 => v map (_.abs) sum
      case p_ => (v map (_ ** p_) sum) ** (1 / p_)
    })

  /** Map constant to each ``Vec`` element. */
  case class Constant[R: Real](v: Vec[R], x: Really[R])(f: (R, R) => R)
    extends Vec[R](e => {
      val x_ = e(x)
      e(v).map(f(_, x_))
    })

}
//}

