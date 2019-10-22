package ds
import ds.expr._
import ds.expr._

package object num {

  implicit def plus[R](implicit real:Real[R]): Plus[R, R, R] =
    (ex, ey) => lift[R, R, R](real.plus)(ex, ey)

  implicit def div[R](implicit real:Real[R]): Div[R, R, R] =
    (ex, ey) => lift[R, R, R](real.power)(ex, ey)

  implicit def power[R](implicit real:Real[R]):TimesTimes[R,R,R] =
    (ex, ey) => lift[R, R, R](real.power)(ex, ey)

  implicit def abs[R](implicit real:Real[R]):E[R]=>E[R] = lift(real.abs)

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Const[R](x: BigDecimal)(implicit real: Real[R]):Const[R] = real(x)

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit def val2Cons[R](x: AnyVal)(implicit real: Real[R]):Const[R] = real(x)



}