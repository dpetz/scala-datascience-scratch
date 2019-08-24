package ds

import ds.algebra.Field

import scala.math.Numeric.{BigDecimalIsFractional, DoubleIsFractional}

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet

package object calc {

  import ds.lina._


  // https://www.scala-lang.org/api/current/scala/math/index.html
  // https://www.scala-lang.org/api/current/scala/Double$.html
  // https://www.scala-lang.org/api/current/scala/math/Numeric.html
  // https://www.scala-lang.org/api/current/scala/math/Ordered.html

  /** https://en.wikipedia.org/wiki/Scalar_field */
  type ScalarField = Vec => Real
  /** https://en.wikipedia.org/wiki/Vector_field */
  type VectorField = Vec => Vec



  // Not used because supported by Double but not by BigDecimal
  trait NotDefined[R] {
     def NaN: R
     def PositiveInfinity: R
     def NegativeInfinity: R
  }

  trait RealMath[R] extends scala.math.Fractional[R]
  {



  }

  trait MathOps[R] {
    def **(y: R): R
  }

  implicit val doubleMath = new  RealMath[Double] with DoubleIsFractional {

    def compare(x:Double, y:Double) = (x - y).toInt



  }

  implicit def mathOps(x:Double) = new MathOps[Double] {
    def **(y: Double) = Math.pow(x,y)

  }

  // https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  implicit val bigDecimalMath = new RealMath[BigDecimal] with BigDecimalIsFractional {

    def compare(x:BigDecimal, y:BigDecimal) = (x - y).toInt

  }

  implicit def mathOps(x:BigDecimal) = new BigDecimalMathOps(x)

  class  BigDecimalMathOps(x:BigDecimal) extends MathOps[BigDecimal] {
    def **(y: BigDecimal) = x.pow(y.toIntExact)
  }


  implicit class Function1Math[X,Y:RealMath](function:X=>Y) {

    val math = implicitly[RealMath[Y]]

    /** Negate real function (one argument) */
    def negate = { x:X => math.negate(function(x)) }

  }

  implicit class FunctionOps[A,B](f:A=>B) {

    def recode(before:B, after:B):A=>B={ x:A =>
      val y = f(x)
      if (y == before) after else y
    }
  }

  trait Tolerance[A] {
    def epsilon:A
    def approx(x:A,y:A):Boolean
  }

  implicit val defaultDoubleTolerance = new Tolerance[Double] {
    val epsilon = 0.00001
    def approx(x: Double, y: Double) = (x - y).abs < epsilon
  }

  implicit val defaultBigDecimalTolerance = new Tolerance[BigDecimal] {
    val epsilon = BigDecimal("0.00001")
    def approx(x: BigDecimal, y: BigDecimal) = (x - y).abs < epsilon
  }

/*
    implicit class DoubleMath(x: Double) {

    def **(y: Double) = Math.pow(x, y)

  }
*/

  implicit class VecMath(v:Vec) {
    //[R:RealMath]
    //val math = implicitly[RealMath[R]

    /** Calculates the p-norm */
    def norm(p:Int):Real = (v dot v) ** p

  }
}
