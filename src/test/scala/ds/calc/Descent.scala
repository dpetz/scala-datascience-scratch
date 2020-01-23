import ds.calc.{Descent, Gradient}
import ds.num.DoubleReal._
import ds.expr.Engine
import ds.expr.Implicits._
import ds.num.Implicits._

class Descent extends ds.PropertySpec {

  val dim:Int = 10
  val dist:Int = 100

  val e = new Engine



  "$dim-dim v*v" should "descend towards (0,..,0)." in {

    val v :Vec[Double] = vec(Seq.fill(dim)(Real.random(-dist,dist)))
    Given("Random start coordinates in [-$dist,$dist): $v")

    val g = Gradient[Double] { v => (v dot v) } { d => d.v(d.i) * 2 }
      
    val min = Descent(g,v).minimize
    
    println("i\tValue\tPosition\n" + "="*30)
    min.history.reverse.foreach {  gd =>
      printf("%s\t%.4f\t",gd.history.size,gd.value)
      println(e(gd.x).format("%.4f"))
    }
   
    e(min.value).toDouble should equal  (0.0 +- 0.1)
  }
}