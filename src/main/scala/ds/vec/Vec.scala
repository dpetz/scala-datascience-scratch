package ds.vec
import parser.{Json, Parser}
import ds.expr.Func.{F1, F2, F3}
import ds.expr._
import ds.num.{Real, Scalar}

import scala.Function.tupled

/** Wraps ``Expr`` evaluating to ``Seq[R]`` for vector infix operations
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
abstract class Vec[R](implicit real:Real[R])extends Expr[Seq[R]] {

  private val vf = Vec.functions(real)
  private val sf = Scalar.functions(real)


  def size:Scalar[R] = vf.size(this)

  def sum: Scalar[R] = vf.sum(this)

  def map(f: F1[R,R]): Vec[R] = vf.map(f,this)

  /** Dot product **/
  def dot(w: Vec[R]): Scalar[R] = vf.dot(this, w)

  /** Calculates the p-norm */
  def norm(p: Int): Scalar[R] = vf.norm(p)(this)

  /** Add elementwise */
  def +(w: Vec[R]): Vec[R] = vf.zip(sf.plus, this, w)

  /** Multiply elementwise */
  def *(w: Vec[R]): Vec[R] = vf.zip(sf.times, this, w)

  /** Divide elementwise */
  def /(w: Vec[R]): Vec[R] = vf.zip(sf.div, this, w)

  /** Substract elementwise */
  def -(w: Vec[R]): Vec[R] = vf.zip(sf.minus, this, w)

  def unary_- : Vec[R] = vf.map(sf.negate, this)

  /** Add constant */
  def +(x: Scalar[R]): Vec[R] = vf.map(sf.plus -> x,this)

  /** Multiply constant */
  def *(x: Scalar[R]): Vec[R] = vf.map(sf.times -> x,this)


}

object Vec {

  def functions[R](implicit real:Real[R]):Functions[R] =
    new Functions[R](real) // @todo Buffer

  class Functions[R:Real](real:Real[R]) {

    type VecOps = F2[Seq[R],Seq[R],Seq[R]]
    type RealOps = F2[Seq[R],Seq[R],R]
    type ScalarField = F1[Seq[R],R]

    /** Scalar functions */
    private val sf = Scalar.functions(real)

    /** Zip vector elements and reduce via function. */
    val zip: F3[(R, R) => R, Seq[R], Seq[R], Seq[R]] =
      Func("each", (f,v,w) => ( v zip w) map (x => f(x._1, x._2)))

    val times:VecOps = zip -> sf.times

    /** Map function expression to every vector element. */
    val map:F2[R=>R,Seq[R],Seq[R]] = Func("map", (f, v) => v map f)

    val sum:ScalarField = Func("sum", _.fold(real.zero)(real.plus))

    val dot:RealOps = times >> sum

    def norm(p:Int):ScalarField  = p match {
      case 1 => (map -> sf.abs) >> sum
      case p:_ => ((map -> (sf.power --> p)) >> sum) >> (sf.power --> (1.0 / p))
    }

    val size:ScalarField = Func("size", _.size)


  }


}

