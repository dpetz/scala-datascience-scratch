package ds.num

import scala.math.Numeric.BigDecimalIsFractional
import scala.util.Random


/**
  * @see https://www.scala-lang.org/api/current/scala/math/BigDecimal.html
  */
package object BigReal {

  type R = BigDecimal
  
  implicit val Real = new ds.num.Real[R]() with BigDecimalIsFractional {

    def compare(x:R, y:R):Int = (x - y).toInt

    def random(min:R=zero, max:R=one): R = min + ((max - min) * Random.nextDouble)

    def json(n:parser.Num):R = n.asBigDecimal

    /**  `x` to the power of `y` with `Double` precision.
      * @todo Consider {@link https://arxiv.org/abs/0908.3030v3} for higher precision. */
    def power(x:R, y: R):R = BigDecimal(scala.math.pow(x.toDouble,y.toDouble))//x.pow(y.toInt)

    def apply(d:Double):R = BigDecimal(d)

    def apply(i:Int):R = BigDecimal(i)

    val precision = BigDecimal(0.0000001)

    def approx(x: R, y: R):Boolean = this.abs(x - y) < precision

    val MAX:R = BigDecimal(Double.MaxValue)

    val MIN:R = BigDecimal(Double.MinValue)

  }

}

