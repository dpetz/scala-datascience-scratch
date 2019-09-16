package ds.num

import scala.math.Numeric.BigDecimalIsFractional
import scala.util.Random


/**
  * @see https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  */
package object BigReal {

  type BD = BigDecimal
  type Real = BD
  type Vec = Seq[BD]

  implicit val Real = new ds.num.Real[BD]() with BigDecimalIsFractional {

    def compare(x:BD, y:BD):Int = (x - y).toInt

    def random = BigDecimal(Random.nextDouble)

    def json(n:parser.Num):BD = n.asBigDecimal

    def power(x:BD, y: BD):BD = x.pow(y.toIntExact)

    def apply(d:Double):BD = BigDecimal(d)

  }

  implicit val Tolerance = new ds.num.Tolerance[BD] {
    val epsilon = BigDecimal("0.00001")
    def approx(x: BD, y: BD): Boolean = (x - y).abs < epsilon
  }

}

