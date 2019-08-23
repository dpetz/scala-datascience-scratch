package ds

import ds.algebra.Precision

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet

package object calc {

  import ds.lina._

  // https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  // https://www.scala-lang.org/api/current/scala/math/index.html
// https://www.scala-lang.org/api/current/scala/Double$.html

  /** https://en.wikipedia.org/wiki/Scalar_field */
  type ScalarField = Vec => Real
  /** https://en.wikipedia.org/wiki/Vector_field */
  type VectorField = Vec => Vec

  trait RealOps[R] {
    def **(y: R): R
    def NaN:R
    def PositiveInfinity:R
  }

  implicit class BigDecimalOps(x: BigDecimal) {
    def **(y: BigDecimal) = x.pow(y.toIntExact)
  }

    implicit class DoubleOps(x: Double) {

    def **(y: Double) = Math.pow(x, y)

  }


  implicit class VecMath(v:Vec) {

    /** Calculates the p-norm */
    def norm(p:Int):Double = (v dot v) ^ p

  }
}
