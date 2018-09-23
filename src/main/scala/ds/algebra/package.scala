package ds

/**
 * @see https://github.com/non/spire
 * @see https://twitter.github.io/algebird/
 */

package object algebra {

	implicit val doubleField = new Field[Double] {
	  val zero = 0.0
	  val one = 1.0
	  def plus(l:Double,r:Double):Double = l+r
	  def negate(v:Double):Double = -v
	  def minus(l:Double, r:Double):Double = l-r
	  def times(l:Double,r:Double):Double = l*r
	  def divide(l:Double,r:Double):Double = l/r
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