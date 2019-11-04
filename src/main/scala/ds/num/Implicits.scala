package ds.num
import ds.expr.{Const, Expr}
import ds.expr.Infix._
import ds.expr.Functions._
import ds.expr.Implicits._

object Implicits {

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

  implicit def negate[R:Real](implicit real:Real[R]):Negate[R] =
    _ flatMap { real.minus(real.zero, _) }


  implicit def approx[R](implicit real:Real[R]): Approx[R] =
    (ex, ey) => lift[R, R, Boolean](real.approx)(ex, ey)

  implicit def compare[R](implicit real:Real[R]): Compare[R] =
    (ex, ey) => expr(lift[R, R, Int](real.compare)(ex, ey))

  /** Convert ``Ìnt`` to ``Expr`` on the fly */
  implicit def big2Const[R](x: BigDecimal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  implicit def valExpr[R](x: AnyVal)(implicit real: Real[R]):Const[R] = Const(real(x))

  /** Convert ``Ìnt``, ``Double`` etc. to ``Expr`` on the fly */
  //implicit def realExpr[R:Real](x: R):Const[R] = Const(x)

}