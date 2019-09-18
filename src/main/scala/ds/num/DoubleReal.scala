package ds.num

import parser.Num

import scala.math.Numeric.DoubleIsFractional
import scala.util.Random

/** Implements [[Real]] with [[Double]] precision. */
package object DoubleReal {

  type R = Double

  implicit val Real = new ds.num.Real[R] with DoubleIsFractional {

    def compare(x:R, y:R): Int = (x - y).toInt

    def random:R = Random.nextDouble

    def json(n:Num): R = n.asDouble

    def power(x:R, y: R):R = Math.pow(x,y)

    def apply(d:R):R = d

    def apply(i:Int):R = i.toDouble

  }


  implicit val Tolerance = new ds.num.Tolerance[R] {
    val epsilon = 0.0000001
    def approx(x: R, y: R):Boolean = (x - y).abs < epsilon
  }


}

