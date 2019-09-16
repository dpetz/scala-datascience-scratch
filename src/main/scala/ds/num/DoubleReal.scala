package ds.num

import parser.Num

import scala.math.Numeric.DoubleIsFractional
import scala.util.Random

/** Implements [[Real]] with [[Double]] precision. */
package object DoubleReal {

  type D = Double
  type Real = D
  type Vec = Seq[D]

  implicit val Real = new ds.num.Real[D] with DoubleIsFractional {

    def compare(x:D, y:D): Int = (x - y).toInt

    def random:D = Random.nextDouble

    def json(n:Num): D = n.asDouble

    def power(x:D, y: D):D = Math.pow(x,y)

    def apply(d:D):D = d

  }


  implicit val Tolerance = new ds.num.Tolerance[D] {
    val epsilon = 0.00001
    def approx(x: D, y: D):Boolean = (x - y).abs < epsilon
  }


}

