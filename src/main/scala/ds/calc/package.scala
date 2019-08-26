package ds

import ds.algebra.{Field, Group}

import scala.math.Numeric.{BigDecimalIsFractional, DoubleIsFractional}
import scala.util.Random

// https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet

package object calc {

  import ds.lina._


  // https://www.scala-lang.org/api/current/scala/math/index.html
  // https://www.scala-lang.org/api/current/scala/Double$.html
  // https://www.scala-lang.org/api/current/scala/math/Numeric.html
  // https://www.scala-lang.org/api/current/scala/math/Ordered.html

  /** Just a shorthand for the common case of doubles.
    * Methods in this class work for other types too if you provide a [[Group]] or [[Field]] where required
    */

  type Real = BigDecimal

  type Vec = Seq[Real]

  /** https://en.wikipedia.org/wiki/Scalar_field */
  type ScalarField = Vec => Real
  /** https://en.wikipedia.org/wiki/Vector_field */
  type VectorField = Vec => Vec



  // Not used because supported by Double but not by BigDecimal
  trait NotDefined[R] {
    val NaN: R
    val PositiveInfinity: R
    val NegativeInfinity: R
  }

  trait RealMath[R] extends scala.math.Fractional[R] {
    def random:R
  }

  trait MathOps[R] {
    def **(y: R): R
  }

  implicit val doubleMath = new  RealMath[Double] with DoubleIsFractional {

    def compare(x:Double, y:Double) = (x - y).toInt

    def random:Double = Random.nextDouble

  }

  implicit class DoubleOps(x:Double) extends MathOps[Double]  {
    def **(y: Double):Double = Math.pow(x,y)
  }

  // https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  implicit val bigDecimalMath = new RealMath[BigDecimal] with BigDecimalIsFractional {

    def compare(x:BigDecimal, y:BigDecimal) = (x - y).toInt

    def random = BigDecimal(Random.nextDouble)

  }

  implicit class BigDecimalOps(x:BigDecimal) extends MathOps[BigDecimal] {
    def **(y: BigDecimal):BigDecimal = x.pow(y.toIntExact)
  }


  implicit class Function1Math[X,Y:RealMath](function:X=>Y) {

    val math = implicitly[RealMath[Y]]

    /** Negate real function (one argument) */
    def negate: X => Y = { x:X => math.negate(function(x)) }

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
    def approx(x: Double, y: Double):Boolean = (x - y).abs < epsilon
  }

  implicit val defaultBigDecimalTolerance = new Tolerance[BigDecimal] {
    val epsilon = BigDecimal("0.00001")
    def approx(x: BigDecimal, y: BigDecimal): Boolean = (x - y).abs < epsilon
  }


  implicit class VecMath(v:Vec) {
    //[R:RealMath]
    //val math = implicitly[RealMath[R]

    /** Calculates the p-norm */
    def norm(p:Int):Real = (v dot v) ** p

  }
}
