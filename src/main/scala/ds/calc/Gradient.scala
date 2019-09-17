package ds.calc

import ds.calc.Func._//{RealValuedFunction, ScalarField, VectorField}
import ds.calc.Gradient.Direction
import ds.lina.VectorUtil._
import ds.num.{Real, Tolerance}
import ds.num.Real.Infix


/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param pd ith partial difference quotient of f at v
  * */
case class Gradient[R:Real](f:ScalarField[R])(pd:Direction[R] => R) extends VectorField[R] {

  /** Computes full gradient on v across all indices */
  def apply(v:Vec[R]):Vec[R] = (v indices) map { i:Int => pd(Direction(v,i)) }

  /** Negate function and partial derivatives */
  def unary_- : Gradient[R] = Gradient(-f)(-pd)

}

object Gradient {

  /** A position vector + a direction index to request a partial derivative */
  case class Direction[R](v:Vec[R],i:Int)

  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * The tiny constant to approximate limit for difference quotient is taken from Precision.
    * */
  def estimate[R:Real](f:ScalarField[R])(implicit p:Tolerance[R]) =
    Gradient(f) { case Direction(v,i) =>
    // https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      val w = v.view.updated (i, (v(i) + p.epsilon) )
      ((f (w) ) - f (v) ) / p.epsilon
  }

}