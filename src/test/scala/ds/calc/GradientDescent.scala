import org.scalatest._
import parser._
import ds.calc._
import ds.lina._

import scala.util.Random 

class GradientDescent extends FlatSpec with OptionValues with Matchers {

  val dim:Int = 3
  val dist:Double=10

  "Descending $dim-dimensional v*v f" should "converge 0 at (0"+",0"*dim+")." in {

    println(s"Random start coordinates in [-$dist,$dist): start")
    val x = Seq.fill(dim)(Random.nextDouble*2*dist-dist) 
    val f = { v:Vec => v dot v }
    val g = Gradient { (i,v) => 2 * v(i) } 
      
    val result = GradientDescent(f,g,x).minimize
    
    println("i\tValue\tPosition\n" + "="*30)
    result.history.reverse.foreach {
      gd => printf("%s\t%.4f\t",gd.history.size,gd.value)
        println(gd.x.format("%.4f"))
    }
   
    True should be (True)
  }
}