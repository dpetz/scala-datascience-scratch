package ds.num

import scala.math.Numeric.DoubleIsFractional
import scala.util.Random

/** Implements [[Real]] with [[Double]] precision. */
package object DoubleReal {

  type R = Double

  implicit val Real = new ds.num.Real[R] with DoubleIsFractional {

    def compare(x:R, y:R): Int = (x - y).toInt

    def random(min:R=0.0,max:R=1.0):R = min + ((max - min) * Random.nextDouble)

    def json(n:parser.Num): R = n.asDouble

    def power(x:R, y: R):R = Math.pow(x,y)

    def apply(d:Double):R = d

    def apply(i:Int):R = i.toDouble

    val precision = 0.0000001

    def approx(x: R, y: R):Boolean = this.abs(x - y) < precision

    val MAX:R = Double.MaxValue

    val MIN:R = Double.MinValue

    /** Exception when dividing by zero for consistency with [[BigDecimal]] */
    override def div(x:R,y:R):R =
      if (y != 0.0) DoubleIsFractional.div(x,y) else throw new ArithmeticException()

  }


}

