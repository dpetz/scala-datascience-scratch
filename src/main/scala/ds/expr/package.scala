package ds

import ds.num.real._

package object expr {

  type E[R] = Expr[R]

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Expr[R](x: BigDecimal)(implicit real:Real[R]):E[R] = new Really(_ => real(x))
  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def int2Expr[R](x: Int)(implicit real:Real[R]):E[R] = new Really(_ => real(x))
  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def dbl2Expr[R](x: Double)(implicit real:Real[R]):E[R] = new Really(_ => real(x))
  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def real2Expr[R](x: R)(implicit real:Real[R]):E[R] = new Really(_ => x)
}