package ds.matrix

import ds.expr.Expr
import ds.num.Real

/** Multiply matrix with real */
case class TimesReal[R](override val m: Matrix[R], x: Expr[R])(implicit r: Real[R])
  extends Map[R](m)(e => r.times(_, e(x)))
