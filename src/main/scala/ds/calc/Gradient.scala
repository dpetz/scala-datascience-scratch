package ds.calc

import ds.lina._

/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param atIndex Computes the ith partial difference quotient of f at v
  * */
case class Gradient(atIndex:Int=>ScalarField) extends VectorField {

  /** Computes full gradient on v across all indices */
  def apply(v:Vec):Vec = (v indices) map (atIndex(_)(v))

  def negate:Gradient = Gradient { (i:Int) => -atIndex(i)(_) }

}

object Gradient {

  /**
   * @param pd Partial Derivative
   */
  def apply( pd:(Int, Vec) => Double ):Gradient =
    Gradient { (i:Int) => (v:Vec) => pd(i,v) }

  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * @param h tiny constant to approximate limit for difference quotient
    * */
  def estimate(f:ScalarField, h:Double = 0.00001) = 
    Gradient { (i:Int) => (v:Vec) => {
        val w = v.view.updated(i,v(i)+h)
        ( f(w) - f(v) ) / h
    }
  }

}
