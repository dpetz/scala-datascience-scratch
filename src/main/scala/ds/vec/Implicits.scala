package ds.vec

import ds.expr.Infix.{Div, Minus, Plus, Times}
import ds.expr.{Const, Expr, Term}
import ds.num.Real
import ds.expr.Implicits._
import ds.num.Implicits._
import scala.Function.tupled

object Implicits {

  type Vec[R] = Expr[Seq[R]]

  /** Wraps a ``Seq[X]`` for infix notations. */
  implicit class VecInfix[X](v:Vec[X]) {

    def apply(i:Int):Expr[X] = Term("ds.vec.apply",v,i) { e => e(v)(i) }

    def size[R](implicit real:Real[R]):Expr[R] = Term("ds.vec.size",v){e => real(e(v).size)}

    /** map */
    def each[Y](f:Expr[X]=>Expr[Y]):Vec[Y] =
      Term("ds.vec.each",v,f) { e => e(v) map (x => e(f(expr(x)))) }

    //def each[Y](f:Y=>Expr[Y]):Vec[Y] =
    //  Term("each",ex,f) { e => e(ex) map (x => e(f(x))) }

    /** reduce */
    def all[Y](f:Vec[X]=>Expr[Y]):Expr[Y] = f(v)

    /** Zip and apply binary operation */
    def zip[Y,Z](w:Vec[Y])(f:(Expr[X],Expr[Y])=>Expr[Z]): Term[Seq[Z]] =
      Term("ds.vec.zip",v,w)(e => ( e(v) zip e(w) ) map (pair => e(f(pair._1,pair._2))))

    def dot(w:Vec[X])(implicit real:Real[X]):Expr[X] = Functions.dot(v, w)

    //def minBy(f:Expr[X]=>X)

    /**
      * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      */
    def update(i:Int, f:Expr[X]=>Expr[X]):Vec[X] =
      Term("ds.vec.update",this,f){ e =>
        e(v).view.updated(i,e(f(e(v)(i))))
      }


    def :*(x:Expr[X])(implicit real:Real[X]):Vec[X] = each(_ * x)

    def :+(x:Expr[X])(implicit real:Real[X]):Vec[X] = each(_ + x)

  }

  implicit def timesVec[R:Real]: Times[Seq[R]] = (v,w) => (v zip w)(_ * _)

  implicit def divVec[R:Real]: Div[Seq[R]] = (v,w) => (v zip w)(_ / _)

  implicit def plusVec[R:Real]: Plus[Seq[R]] = (v,w) => (v zip w)(_ + _)

  implicit def minusVec[R:Real]: Minus[Seq[R]] = (v,w) => (v zip w)(_ - _)

  implicit def vec[T](s:Seq[T]): Vec[T] = expr(s)

  implicit def lift[T](s:Seq[Expr[T]]): Vec[T] = Term("ds.vec.lift",s) { e => s.map(e(_)) }

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
