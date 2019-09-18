package ds.num

/** Abstracts numeric operations from a specifc number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within [0,1) */
  def random:R

  /** Parse from json number. */
  def json(n:parser.Num):R

  /** x to the power of y */
  def power(x:R,y:R):R

  /** Convert from double */
  def apply(d:Double):R

  /** Convert from integer */
  def apply(i:Int):R

  /** Tiny constant used in approximations.
    * @see ~, ds.calc.Gradient
    */
  def precision:R

  /** Approximately equal
    * @return Difference is at most [[precision]] */
  def approx(x:R,y:R):Boolean

  /** Huge number treated as biggest possible
    * For a [[Double]] it is the actual highest possible number.
    * For [[BigDecimal]] it is treated as if for consistency */
  def MAX:R

  /** Opposite of [[MAX]] */
  def MIN:R

}

object Real {

  /** Infix operators based on ds.math.Real */
  implicit class RealInfix[R:Real](x:R)(implicit real:Real[R]) {

    def **(y: R): R = real.power(x,y)
    def +(y: R): R = real.minus(x,y)
    def -(y: R): R = real.minus(x,y)
    def *(y: R): R = real.times(x,y)
    def /(y: R): R = real.div(x,y)
    def ~(y: R): Boolean = real.approx(x,y)

  }

  /** Extend [[RealInfix]] (and elsewhere) operations extend to integer parameters. */
  implicit def int2Real[R](x: Int)(implicit real:Real[R]):R = real(x)
  /** Extend [[RealInfix]] (and elsewhere) operations to double parameters. */
  implicit def double2Real[R](x: Double)(implicit real:Real[R]):R = real(x)

}
