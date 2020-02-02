package ds.math

import ds.expr.Infix.{Div, Minus, Plus, Times}

import ds.expr.{expr,Expr, Term, Symbol}
import ds.num.Real
import ds.num.Implicits

import scala.Function.tupled

object Vector {

  type E[R] = Expr[R]
  type S[R] = Seq[R]
  type Vec[R] = E[S[R]]


  implicit

  implicit class Ops[X](v:Vec[X]) {

    /** map each element*/
    def each[Y](f: E[X => Y]): Vec[Y] = Term(v, f) {
      (v: S[X], f: X => Y) => v map (x => f(x))
    } name "vec.each"

    /** reduce */
    def all[Y](f:E[S[X]=>Y]):E[Y] = Term(v, f) {
      (v: S[X], f:S[X] => Y) => f(v)
    } name "vec.all"

    /** select element by index */
    def apply(i:E[Int]):E[X] = Term(v,i) {
      (v:S[X],i:Int) => v(i)
    } name ( "vec.apply")


    def size[X](implicit real:Real[X]):E[Int] =
      Term(v) { v:S[X] => v.size } name "vec.size"


    /** Zip and apply binary operation */
    def zip[Y,Z](w:Vec[Y])(f:E[(X,Y)=>Z]): Vec[Z] = Term(v,w,f) {
      ( v:S[X], w:S[Y], f:(X,Y)=>Z ) => ( v zip w ) map (pair => f(pair._1,pair._2))
    } name "vec.zip"

    /**
      * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      */
    def update(i:E[Int])(f:E[X=>X]):Vec[X] = Term (v,i,f){ // [X,Int,X=>X]
      (v:S[X],i:Int,f:X=>X) => (v.view.updated(i,f(v(i)))).toSeq
    } name "vec.update"
  }


  /** Wraps a ``Seq[X]`` for infix notations. */
  implicit class MathOps[R](v:Vec[R])(implicit real:Real[R]) {

    def sum: E[R] = Term(v) {
      v:Seq[R] => v.foldLeft(real.zero)(real.plus)
    } name "vec.sum"

    def dot(w: Vec[R]) : E[R] = (v * w).sum name "vec.dot"


    private val _1 = Symbol[R]("_1")

    def norm(p: Int): E[R] = {
      val expr = p match {
        case 1 => v.each(real.abs _).sum
        case p => v.each( _1 ** p of _1 ).sum ** (1 / p)
      }
      expr name s"vec.norm($p)"
    }


    /** Elementwise addition of a scalar. */
    def :+(x:Expr[R]):Vec[R] = v each (_1 + x of _1)

    //def minBy(f:Expr[X]=>X)

    /** Elementwise multiplication with a scalar. */
    def :*(x:Expr[R]):Vec[R] = v each (_1 * x of _1)
  }

  implicit def timesVec[R:Real]: Times[Seq[R]] = (v,w) => (v zip w)(implicitly[R].+)

  implicit def divVec[R:Real]: Div[Seq[R]] = (v,w) => (v zip w)(_ / _)

  implicit def plusVec[R:Real]: Plus[Seq[R]] = (v,w) => (v zip w)(_ + _)

  implicit def minusVec[R:Real]: Minus[Seq[R]] = (v,w) => (v zip w)(_ - _)

  implicit def vec[T](s:Seq[T]): Vec[T] = expr(s)

  implicit def lift[T](s:Seq[Expr[T]]): Vec[T] = Term("ds.vec.lift")(s) { e => s.map(e(_)) }

  implicit def convertElements[X,Y](s: Seq[X])(implicit f: X => Y):Seq[Y] = s.map(f)

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




}
