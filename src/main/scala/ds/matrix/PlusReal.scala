package ds.matrix

import ds.expr.Expr
import ds.num.Real

case class PlusReal[R](override val m: Matrix[R], x: Expr[R])(implicit r: Real[R])
  extends Map[R](m)(e => r.plus(_, e(x)))
