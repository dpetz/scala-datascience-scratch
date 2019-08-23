package ds.calc

import ds.algebra.Precision
import ds.lina._



/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param pd ith partial difference quotient of f at v
  * */
case class Gradient(f:ScalarField)(pd:(Vec, Int) => Double) extends VectorField {

  /** Computes full gradient on v across all indices */
  def apply(v:Vec):Vec = (v indices) map (pd(v,_))

  def negate:Gradient = Gradient(f) { (v:Vec,i:Int) => -pd(v,i) }

}

object Gradient {


  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * The tiny constant to approximate limit for difference quotient is taken from Precision.
    * */
  def estimate(f:ScalarField)(implicit p:Precision[Double]) = Gradient(f) {
    // val epsilon = implicitly[Precision[Double]].epsilon.toDouble
    // https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
    (v:Vec,i:Int) => {
        val w = v.view.updated(i, (v(i) + p.epsilon))
        ( f(w) - f(v) ) / p.epsilon
    }
  }

}