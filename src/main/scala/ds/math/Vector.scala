package ds.math

import ds.expr.Infix.{Div, Minus, Plus, Times}
import ds.expr.{Engine, Expr, Expressible, Term, expr}
import ds.num.Real
import ds.num.Functions.{Plus, abs}
import ds.num.Implicits._

import scala.Function.tupled

object Vector {

  type E[R] = Expr[R]
  type Vec[R] = E[Seq[R]]


  case class Sum[R](v: Vec[R])(implicit real:Real[R]) extends Term[R] {
    def eval(e:Engine) : R = e(v).foldLeft(real.zero)(real.plus)
  }




  object Vector {

    /** map */
    def each[X,Y](v: Vec[X], f:Expr[X=>Y]) : Term[Vec[Y]] =
      def eval(e:Engine) : Vec[Y] = e(v) map (x => e(f(x)))
    }


  }



  case class Dot[R:Real](v: Vec[R], w: Vec[R]) extends Term[R] with Expressible[R] {
    def express : Expr[R] = Sum(v * w)
  }



  /** Wraps a ``Seq[X]`` for infix notations. */
  implicit class Infix[X](v:Vec[X]) {

    def norm[R](p: Int)(v: Vec[R])(implicit real:Real[R]): Expr[R] =
      Name("vec.norm") { p match {
        case 1 => v.each(abs).all(sum[R])
        case p => v.each((x: Expr[R]) => x ** p).all(sum[R]) ** (1 / p)
      }}

    def apply(i:E[Int]):E[X] =
      Term(v,i) ( Name("vec.apply") { e => (v,i) => e(v)(i) } )

    def size[R](implicit real:Real[R]):Expr[R] =
      Term("ds.vec.size")(v){e => real(e(v).size)}



    /** reduce */
    def all[Y](f:Vec[X]=>Expr[Y]):Expr[Y] = f(v)

    /** Zip and apply binary operation */
    def zip[Y,Z](w:Vec[Y])(f:(Expr[X],Expr[Y])=>Expr[Z]): Term[Seq[Z]] =
      Term("ds.vec.zip")(v,w)(e => ( e(v) zip e(w) ) map (pair => e(f(pair._1,pair._2))))


    //def minBy(f:Expr[X]=>X)

    /**
      * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      */
    def update(i:Int, f:Expr[X]=>Expr[X]):Vec[X] =
      Term("ds.vec.update")(this,f){ e =>
        e(v).view.updated(i,e(f(e(v)(i))))
      }



    /** Elementwise addition of a scalar. */
    def :+(x:Expr[X])(implicit real:Real[X]):Vec[X] = each(_ + x)

    /** Elementwise multiplication with a scalar. */
    def :*(x:Expr[X])(implicit real:Real[X]):Vec[X] = Each(v,Plus(x,_))
  }

  implicit def timesVec[R:Real]: Times[Seq[R]] = (v,w) => (v zip w)(_ * _)

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
