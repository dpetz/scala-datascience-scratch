package ds

import ds.algebra.Precision
import ds.lina._
//import scala.math.BigDecimal

/**
 * @see https://github.com/non/spire
 * @see https://twitter.github.io/algebird/
 */

package object algebra {


	//case class DoublePrecision(epsilon:Double) extends Precision[Double]

	trait Precision[A] {
		def epsilon:A
		def approx(x:A,y:A):Boolean
	}

	implicit val defaultDoublePrecision = new Precision[Double] {
		val epsilon = 0.00001
		def approx(x: Double, y: Double) = (x - y).abs < epsilon
	}

	implicit val defaultBigDecimalPrecision = new Precision[BigDecimal] {
		val epsilon = BigDecimal(0.00001)
		def approx(x: BigDecimal, y: BigDecimal) = (x - y).abs < epsilon
	}


	implicit val bigDecimalField = new Field[BigDecimal] {
		val zero = BigDecimal(0.0)
		val one = BigDecimal(1.0)
		def plus(l:BigDecimal,r:BigDecimal):BigDecimal = l+r
		def negate(v:BigDecimal):BigDecimal = -v
		def minus(l:BigDecimal, r:BigDecimal):BigDecimal = l-r
		def times(l:BigDecimal,r:BigDecimal):BigDecimal = l*r
		def divide(l:BigDecimal,r:BigDecimal):BigDecimal =
			if (r != zero) l/r else
			if (l==zero) zero else
				(l + defaultBigDecimalPrecision.epsilon) / (r + defaultBigDecimalPrecision.epsilon)
		def inverse(v:BigDecimal):BigDecimal = 1/v
	}

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

	implicit class Function1Math[X,Y:Field](function:X=>Y) {

	val algebra = implicitly[Field[Y]]

		 /** Negate real function (one argument) */
		def negate = { x:X => algebra.negate(function(x)) }

	}

  implicit class FunctionOps[A,B](f:A=>B) {
  	
  	def recode(before:B, after:B):A=>B={ x:A =>
  		val y = f(x)
  		if (y == before) after else y
  	}
  }


	/*
	implicit class Function2Math[X,Z,Y:Field](function:(X,Z)=>Y) {

		val algebra = implicitly[Field[Y]]

		/** Negate real function (one argument) */
		def negate = { (x:X,z:Z) => algebra.negate(function(x,z)) }
	}
	*/

}