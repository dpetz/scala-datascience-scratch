import ds.num.BigReal._
import ds.num.Real._
import ds.calc.Gradient.Direction
import ds.lina.Vec
import org.scalacheck.Gen
import ds.lina.Vec._

/**
  * Works surprisingly well for `ds.num.BigReal._` and surprisingly bad for `ds.num.DoubleReal._`
  * Large `Double` are too sparse for gradient estimation so most partial derivatives are `0.0`.
  * @see https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md
  */
class Gradient extends ds.PropertySpec {

    val polynomial_gradient:ds.calc.Gradient[R] = ds.calc.Gradient {
      v:Vec[R] => v.indexed map { e => e.x ** e.i } total
    } {
      d:Direction[R] => d.i * (  d.v(d.i) ** Real(d.i-1) )
    }

    def estimation_error(g:ds.calc.Gradient[R], v:Vec[R]): Double = {
      Given("random x:" + v)

      val g_v = g(v)
      When("analytical gradient:" + g_v)

      val g_est = (ds.calc.Gradient.estimate(g.f))
      val g_est_v = g_est(v)
      Then("gradient estimated:" + g_est_v)

      val err = (g_v zip g_est_v) map { case (act,est) =>
        if(act.abs < 1) (est - act) else (est - act + Real.precision) / (act + Real.precision)
      }
      And("errors (abs if in [-1,1] else %): " + err.json)
      val err_avg = (err norm 1) / v.size
      And("average errors (1 norm): " + err_avg + "\n")

      err_avg.toDouble
    }

  "Polynomial gradient" should "estimate well for a specific x" in {
    val sample_x = Vec(
      "[6.072721482157302,8.176355644038862,7.825980307019224,1.81684073553305,2.6316372621853437,2.093666610582376,4.40047512448909,6.704175154754859,9.153798233685135,9.660464204276792,9.32321245761975,3.4561774206862172,2.2714081280371103,6.041985100712073,5.141961580442019,9.94413741878291,2.509043737599328,6.0715246880103795]"
    )
        estimation_error(polynomial_gradient, sample_x) should be <= 0.00001
  }

  it should "estimate well for x = [0]" in {
    estimation_error(polynomial_gradient, List(Real.one)) should be <= 0.00001

  }

  it should "estimate well for 1-10 x_i from [0,100]." in {

    val vs = Gen.nonEmptyContainerOf[Array, Double](Gen.choose(1.0, 10.0))

    forAll(vs, minSize(0), sizeRange(100)) { a: Array[Double] => //
      val v = a.map(Real(_)).toSeq
      estimation_error(polynomial_gradient, v) should be <= (.00001 * a.size)
    }
  }
 }
