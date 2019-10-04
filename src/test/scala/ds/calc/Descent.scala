import ds.num.DoubleReal._
import ds.vec.VecInfix._
import ds.calc.{Descent, Gradient}

class Descent extends ds.PropertySpec {

  val dim:Int = 10
  val dist:Int = 100

  "$dim-dim v*v" should "descend towards (0,..,0)." in {

    val v :Vec[R] = Seq.fill(dim)(Real.random(-dist,dist))
    Given("Random start coordinates in [-$dist,$dist): $v")

    val g = Gradient { v => v dot v } { d => 2 * d.v(d.i) }
      
    val min = Descent(g,v).minimize
    
    println("i\tValue\tPosition\n" + "="*30)
    min.history.reverse.foreach {  gd =>
      printf("%s\t%.4f\t",gd.history.size,gd.value)
      println(gd.x.format("%.4f"))
    }
   
    min.value.toDouble should equal  (0.0 +- 0.1)
  }
}