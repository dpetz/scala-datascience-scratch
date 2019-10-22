package ds
import ds.expr.Infix._
import ds.expr._

package object num {

  implicit def plus[R](implicit real:Real[R]): Plus[R] =
    (ex, ey) => lift[R, R, R](real.plus)(ex, ey)

  implicit def minus[R](implicit real:Real[R]): Minus[R] =
    (ex, ey) => lift[R, R, R](real.minus)(ex, ey)

  implicit def times[R](implicit real:Real[R]): Times[R] =
    (ex, ey) => lift[R, R, R](real.times)(ex, ey)

  implicit def div[R](implicit real:Real[R]): Div[R] =
    (ex, ey) => lift[R, R, R](real.power)(ex, ey)

  implicit def power[R](implicit real:Real[R]):TimesTimes[R] =
    (ex, ey) => lift[R, R, R](real.power)(ex, ey)

  def abs[R](implicit real:Real[R]):E[R]=>E[R] = lift(real.abs)

  implicit def inverse[R:Real]:E[R]=>E[R] = implicitly[Real[R]].one / _

  implicit def negate[R:Real]:Negate[R] = implicitly[Real[R]].zero - _


  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Const[R](x: BigDecimal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit def val2Cons[R](x: AnyVal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  //implicit def real2Cons[R:Real](x: R):Const[R] = Const(x)

}