package ds.num

import ds.num.Real._

class AnyReal[R] (implicit real:Real[R], tolerance:ds.num.Tolerance[R])  extends ds.SimpleSpec  {

  "1" should "equal one + zero" in {
    real.zero + real.one == real(1)
  }

  it should "approximate 1 / 3 * 3" in {
    tolerance.approx( real(1), (real(1) / 3) * 3) shouldBe true
  }

  val x:R = real.random

  s"$x" should "equal zero when multiplied with zero" in {
    (x * real.zero ) shouldBe real(0)
  }

  it should "should throw exception when divided by zero" in {
    an [Exception] should be thrownBy (x / real.zero)
  }

  it should "should not through exception when divided by (zero + epsilon)" in {
    noException should be thrownBy  (x / (real.zero + tolerance.epsilon))
  }


}
