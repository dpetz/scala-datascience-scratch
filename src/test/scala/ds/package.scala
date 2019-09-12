import org.scalatest.{FlatSpec, GivenWhenThen, Matchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

package object ds {

  trait PropertySpec extends FlatSpec
    with OptionValues
    with Matchers
    with ScalaCheckPropertyChecks
    with GivenWhenThen

}
