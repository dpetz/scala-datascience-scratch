package ds.calc

import ds.lina.Vec._
import ds.num.Real


// https://www.scala-lang.org/api/current/scala/math/index.html
// https://www.scala-lang.org/api/current/scala/Double$.html
// https://www.scala-lang.org/api/current/scala/math/Numeric.html
// https://www.scala-lang.org/api/current/scala/math/Ordered.html

 object Func {

  /** https://en.wikipedia.org/wiki/Scalar_field */
  type ScalarField[R] = Vec[R] => R
  /** https://en.wikipedia.org/wiki/Vector_field */
  type VectorField[R] = Vec[R] => Vec[R]

  implicit class RealValuedFunction[X, R:Real](f: X => R) {

    val real = implicitly[Real[R]]

    def -(y:R):X => R = { x: X => real.minus(f(x), y) }

    def /(y:R):X => R = { x: X => real.div(f(x), y) }

    /** Negate real function (one argument) */
    def unary_- : X => R = { x: X => real.negate(f(x)) }

  }

   implicit class VecValuedFunction[X, R: Real](function: X => Vec[R]) {

     val real = implicitly[Real[R]]

     /** Negate real function (one argument) */
     def unary_- : X => Vec[R] = { x: X => -function(x) }

   }

  implicit class FunctionOps[A, B](f: A => B) {

    def recode(before: B, after: B): A => B = { x: A =>
      val y = f(x)
      if (y == before) after else y
    }
  }

}


