package ds.num
import ds.expr.Const
import ds.expr.Infix._

object Implicits {

  implicit def plus[R](implicit real:Real[R]): Plus[R] =
    (ex, ey) => real.plus(ex, ey)

  implicit def minus[R](implicit real:Real[R]): Minus[R] =
    (ex, ey) => real.minus(ex, ey)

  implicit def times[R](implicit real:Real[R]): Times[R] =
    (ex, ey) => real.times(ex, ey)

  implicit def div[R](implicit real:Real[R]): Div[R] =
    (ex, ey) => real.power(ex, ey)

  implicit def power[R](implicit real:Real[R]):TimesTimes[R] =
    (ex, ey) => real.power(ex, ey)

  implicit def negate[R:Real](implicit real:Real[R]):Negate[R] =
    ex => real.minus(real.zero, ex)


  implicit def approx[R](implicit real:Real[R]): Approx[R] =
    (ex, ey) => real.approx(ex, ey)

  implicit def compare[R](implicit real:Real[R]): Compare[R] =
    (ex, ey) => real.compare(ex, ey)

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Const[R](x: BigDecimal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit def valExpr[R](x: AnyVal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  //implicit def realExpr[R:Real](x: R):Const[R] = Const(x)

}