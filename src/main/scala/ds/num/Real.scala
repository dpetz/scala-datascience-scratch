package ds.num

import parser.Json

/** Abstracts numeric operations from a specific number representation
  * such as Double or BigDecimal. */
trait Real[R] extends scala.math.Fractional[R] {

  /** Create random number within `[min,max)` */
  def random(min:R=zero,max:R=one):R

  /** Parse from json number. */
  def json(n:Json.Num):R

  /** x to the power of y */
  def power(x:R,y:R):R

  /** Convert from a value, like double or int*/
  def apply(x:AnyVal):R


  /** Convert from BigDecimal */
  def apply(b:BigDecimal):R


  /** Tiny constant used in approximations.
    * @see ~, ds.calc.Gradient
    */
  def precision:R

  /** Approximately equal
    * @return Difference is at most [[precision]] */
  def approx(x:R,y:R):Boolean

  /** Huge number treated as largest possible
    * For a [[Double]] it is the actual highest possible number.
    * For [[BigDecimal]] it is treated as if it where for consistency */
  def MAX:R

  /** Opposite of [[MAX]] */
  def MIN:R

}

