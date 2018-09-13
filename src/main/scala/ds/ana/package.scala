package ds

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet

package object ana {

  import ds.algebra._

  implicit val algebra = DoubleField()


  	// @todo: how to generalize number of arguments?

	implicit class Function1Math[X,Y:Field](function:X=>Y) {

    	val algebra = implicitly[Field[Y]]

		 /** Negate real function (one argument) */
		def negate = { x:X => algebra.negate(function(x)) }


		def wrap(outer:Y=>Y):(X=>Y) = { x:X => outer(function(x)) }
	}



	implicit class Function2Math[X,Z,Y:Field](function:(X,Z)=>Y) {

    	val algebra = implicitly[Field[Y]]

		/** Negate real function (one argument) */
		def negate = { (x:X,z:Z) => algebra.negate(function(x,z)) }
	}

  /** Negate vector function */
  //def negateVecFunc[T](f:T => Vec):T=>Vec= { v:T => f(v).map {-_} }


  /* Why not working?
  def negate[T,R](f:T => R):(T=>R) = f match {
      case vf:(T=>Vec)    => v:T => vf(v).map {-_}
      case df:(T=>Double) =>  v:T => -df(v)
      case _ => throw new IllegalArgumentException(s"Not evaluating to Vec or Double: $f")
  }
  */
}
