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

    val precision = 0.0000001

    def approx(x: R, y: R):Boolean = this.abs(x - y) < precision

    def MAX:R = Double.MaxValue

    def MIN:R = Double.MinValue

    /** Exception when dividing by zero for consistency with [[BigDecimal]] */
    override def div(x:R,y:R):R =
      if (y != 0.0) DoubleIsFractional.div(x,y) else throw new ArithmeticException()

  }


}

