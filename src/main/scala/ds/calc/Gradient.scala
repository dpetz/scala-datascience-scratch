package ds.calc

import ds.calc.Gradient.Direction
import ds.expr._
import ds.num._
import ds.vec._


//abstract class VectorField[R:Real] extends Func[Vec[R],Vec[R]]
//abstract class ScalarField[R:Real] extends Really[R] with Func[Vec[R],R]


/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param pd ith partial difference quotient of f at v
  * */
case class Gradient[R:Real](f:Seq[R]=>R, pd:Direction[R] => R)(v:Expr[Seq[R]]) extends (Expr[Seq[R]] => Expr[Seq[R]])  {

  /** Computes full gradient on v across all indices */
  def apply(e:Engine):Seq[R] =
    (e(v) indices) map { i:Int => pd(Direction(v,i)) }

  /** Negate function and partial derivatives */
  def unary_- : Gradient[R] = Gradient(-f,-pd)

}

object Gradient {

  /** A position vector + a direction index to request a partial derivative */
  case class Direction[R](v:Expr[Seq[R]], i:Int)

  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * The tiny constant to approximate limit for difference quotient is taken from Precision.
    * */
  def estimate[R](f:Seq[R]=>R)(implicit real:Real[R]) =
    Gradient(f, { case Direction(v,i) =>
      val w = v update (i, (x:Expr[R]) => x + real.precision )
      ( f(w) - f(v) ) / real.precision
    }

}