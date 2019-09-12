package ds

import ds.lina.Vec.Math
import ds.num.Real
import ds.num.Real.Infix

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
// https://www.scala-lang.org/api/current/scala/math/index.html
// https://www.scala-lang.org/api/current/scala/Double$.html
// https://www.scala-lang.org/api/current/scala/math/Numeric.html
// https://www.scala-lang.org/api/current/scala/math/Ordered.html

package object calc {


  /** https://en.wikipedia.org/wiki/Scalar_field */
  //type ScalarField[R] = Vec[R] => Real[R]
  /** https://en.wikipedia.org/wiki/Vector_field */
  //type VectorField[R] = Vec[R] => Vec[R]

  implicit class Function1Math[X, Y: Real](function: X => Y) {

    val real = implicitly[Real[Y]]

    /** Negate real function (one argument) */
    def negate: X => Y = { x: X => real.negate(function(x)) }

  }

  implicit class FunctionOps[A, B](f: A => B) {

    def recode(before: B, after: B): A => B = { x: A =>
      val y = f(x)
      if (y == before) after else y
    }
  }

}


