import Function.tupled
import org.scalatest._
import ds.calc._
import ds.lina._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// https://github.com/rickynils/scalacheck/blob/master/doc/UserGuide.md


trait PropertySpec extends FlatSpec
  with OptionValues
  with Matchers
  with ScalaCheckPropertyChecks
  with GivenWhenThen

class Gradient extends PropertySpec {

    val polynomial_gradient = Gradient {
      v => v.indexed map { e => e.x^e.i } sum
    } {
      (x,i) => i* ( x(i)^(i-1) )
    }


    def estimation_error(g:ds.calc.Gradient, v:Vec): Double = {
      Given("random x:" + v.json)

      val g_v = g(v)
      When("analytical gradient:" + g_v.json)

      val g_est = (Gradient.estimate(g.f))
      val g_est_v = g_est(v)
      Then("gradient estimated:" + g_est_v.json)

      val err = ( (g_v - g_est_v ) / g_v )
      And("% errors: " + err.json)
      val err_avg = err norm (1)
      And("average % errors: " + err_avg + "\n")

      return err_avg
    }

  "Polynomial gradient" should "estimate well for a specific x" in {
    val sample_x =
      List(6.072721482157302,8.176355644038862,7.825980307019224,1.81684073553305,2.6316372621853437,2.093666610582376,4.40047512448909,6.704175154754859,9.153798233685135,9.660464204276792,9.32321245761975,3.4561774206862172,2.2714081280371103,6.041985100712073,5.141961580442019,9.94413741878291,2.509043737599328,6.0715246880103795)
    estimation_error(polynomial_gradient, sample_x) should be < 100.0
  }


  ignore should "estimate well for x = [0]" in {
    estimation_error(polynomial_gradient, List(0)) should be < 1.0

  }

  ignore should "estimate well for 1-10 x_i from [0,100]." in {

    val vs = Gen.nonEmptyContainerOf[Array, Double](Gen.choose(1.0, 10.0))

    forAll(vs, minSize(0), sizeRange(100)) { a: Array[Double] => //
      estimation_error(polynomial_gradient, a.toSeq) should be < (a.size * .00001)
    }
  }





 }
