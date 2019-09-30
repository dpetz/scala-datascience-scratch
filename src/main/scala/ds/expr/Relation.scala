package ds.expr

import ds.num.Real.Real

/** Expression in two arguments evaluating to a ``Boolean`` */
 class Relation[R:Real](val x:E[R], val y:E[R])(val eval:Engine[R] => Boolean) extends Expr[Boolean]