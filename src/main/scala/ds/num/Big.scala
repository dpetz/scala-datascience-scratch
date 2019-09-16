package ds.num

import parser.Json.Parsers
import parser.{Num, Parser}

import scala.math.Numeric.BigDecimalIsFractional
import scala.util.Random


package object Big {

  type Real = BigDecimal

  type Vec = Seq[Real]

  // https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  implicit val Real = new ds.num.Real[BigDecimal] with BigDecimalIsFractional {

    def compare(x:BigDecimal, y:BigDecimal) = (x - y).toInt

    def random = BigDecimal(Random.nextDouble)

    def json(n:Num):BigDecimal = n.asBigDecimal

    def power(x:BigDecimal, y: BigDecimal):BigDecimal = x.pow(y.toIntExact)

  }

  // def toBigDecimal(n:json.Num) =

  implicit val Tolerance = new ds.num.Tolerance[BigDecimal] {
    val epsilon = BigDecimal("0.00001")
    def approx(x: BigDecimal, y: BigDecimal): Boolean = (x - y).abs < epsilon
  }

}

