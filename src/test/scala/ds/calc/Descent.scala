import org.scalatest._
import ds.calc._
import ds.lina._
import scala.language.implicitConversions

import scala.math.Numeric.{BigDecimalIsFractional, DoubleIsFractional}


class Descent[Real:RealMath] extends FlatSpec with OptionValues with Matchers {

  val math = implicitly[RealMath[Real]]

  val dim:Real = math.fromInt(10)
  val dist:Real = math.fromInt(100)

  "Descending $dim-dimensional v*v" should "converge 0 at (0"+",0"* dim.toInt +")." in {

    val v = Seq.fill(dim)(math.random).map(_ * math.fromInt(2) * dist - dist)
    println(s"Random start coordinates in [-$dist,$dist): $v")

    val g = Gradient {
      v => v dot v
    } {
      (v,i) => 2 * v(i)
    }
      
    val min = Descent(g,v).minimize
    
    println("i\tValue\tPosition\n" + "="*30)
    min.history.reverse.foreach {
      gd => printf("%s\t%.4f\t",gd.history.size,gd.v)
        println(gd.x.format("%.4f"))
    }
   
    min.v.toDouble should equal  (0.0 +- 0.1)
  }
}