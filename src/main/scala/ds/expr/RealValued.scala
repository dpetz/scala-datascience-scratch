package ds.expr

/** Expression evaluating to a real number. */
class RealValued[R](val eval:Engine[R] => R)  extends E[R]{

  /** @see Real.power */
  def **(y: E[R]): E[R] = new RealValued[R](e => e.real.power(e(this),e(y)))
  /** @see Real.plus */
  def +(y: E[R]): E[R] = new RealValued[R](e => e.plus(e(this),e(y)))
  /** @see Real.minus */
  def -(y: E[R]): E[R] = new RealValued[R](e => e.minus(e(this),e(y)))
  /** @see Real.times */
  def *(y: E[R]): E[R] = new RealValued[R](e => e.times(e(this),e(y)))
  /** @see Real.div */
  def /(y: E[R]): E[R] = new RealValued[R](e => e.div(e(this),e(y)))
  /** @see Real.approx */
  def ~(y: E[R]): E[Boolean] = Relation(this,y)(_.approx(_,_))

}
