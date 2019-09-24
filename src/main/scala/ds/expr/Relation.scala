package ds.expr

import ds.num.real.Real

/** Expression in two arguments evaluating to a ``Boolean`` */
case class Relation[R](x:E[R], y:E[R])(f:(Real[R],R,R) => Boolean) extends E[Boolean] {
  def eval(e:Engine[R]):Boolean = f(e.real,e(x),e(y))
}
