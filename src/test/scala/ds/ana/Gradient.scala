import org.scalatest._
import io._
import math.ana._
import scala.util.Random 

class Gradient extends FlatSpec with OptionValues with Matchers {

/** Call test to see it at work */


  "Gradient example " should "run" in {

    println("Creating vector with 10 random integers between 1 and 10 ...")
    println("Estimate gradient for function x_0^0 + x_1^1 + x_2^2 + ... + x_9^9")

    def sumPowersToIndex(v:Seq[Double])=
      v.zipWithIndex.map { case (x,i) => Math.pow(x,i) } .sum

    val v = Seq.fill(10)(Random.nextInt(10) + 1.0)
    val g = Gradient(sumPowersToIndex)(v)

    println("Vector=(" + v.map(_.toInt).mkString(",") +
      f"). Function value=${sumPowersToIndex(v)}%.2f\nv\tEstimate\tError")

    for (i <- 0 to 9) printf("%.1f\t%.1f\t%.4f\n" , v(i), g(i), g(i)-i*Math.pow(v(i),i-1))

    True should be (True)

    }
  }

