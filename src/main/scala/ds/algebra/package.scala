package ds

import ds.lina._
//import scala.math.BigDecimal

/**
 * @see https://github.com/non/spire
 * @see https://twitter.github.io/algebird/
 */

package object algebra {


	//case class DoublePrecision(epsilon:Double) extends Precision[Double]

/*

	implicit val doubleField = new Field[Double] {
	  val zero = 0.0
	  val one = 1.0
	  def plus(l:Double,r:Double):Double = l+r
	  def negate(v:Double):Double = -v
	  def minus(l:Double, r:Double):Double = l-r
	  def times(l:Double,r:Double):Double = l*r
	  def divide(l:Double,r:Double):Double =
      if (r!=0.0) l/r else
      if (l==0.0) 0.0 else
        (l + defaultDoublePrecision.epsilon) / (r + defaultDoublePrecision.epsilon)
	  def inverse(v:Double):Double = 1/v
	}

*/

	/*
	implicit class Function2Math[X,Z,Y:Field](function:(X,Z)=>Y) {

		val algebra = implicitly[Field[Y]]

		/** Negate real function (one argument) */
		def negate = { (x:X,z:Z) => algebra.negate(function(x,z)) }
	}
	*/

}