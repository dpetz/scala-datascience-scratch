package ds
import ds.expr.{Engine, Expr}

package object num {

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class BigScalar[R: Real](x: BigDecimal)(implicit real: Real[R]) extends Scalar[R] {
    def eval(e: Engine): R = real(x)
  }

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit class ValScalar[R: Real](x: AnyVal)(implicit real: Real[R]) extends Scalar[R] {
    def eval(e: Engine): R = real(x)
  }

  implicit def val2Real[R](x: AnyVal)(implicit real: Real[R]):R = real(x)

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit class RealScalar[R: Real](x: R) extends Scalar[R] {
    def eval(e: Engine): R = x
  }

  implicit def expr2Scalar[R:Real](expr: Expr[R]):Scalar[R] = new Scalar[R] {
    def eval(e: Engine): R = e(expr)
    override def inputs = List(expr)
  }

}