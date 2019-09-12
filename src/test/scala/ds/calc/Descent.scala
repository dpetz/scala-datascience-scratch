import ds.calc._
import ds.lina._


class Descent(implicit math:Real[Real]) extends ds.PropertySpec {

  val dim:Int = 10
  val dist:Int = 100

  "$dim-dim v*v" should "descend towards (0,..,0)." in {

    val v :Seq[Real] = Seq.fill(dim)(math.random).map(_ * 2 * dist - dist)
    Given("Random start coordinates in [-$dist,$dist): $v")

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