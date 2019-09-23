package ds.expr

import ds.num.Real

trait Expr[R]


object Expr {

  type E[R] = Expr[R]

  /** References [[Real]] function of type `(R,R) => R` such as ``Real.plus` */
  abstract class Arithmetic[R] (val f:Real[R] => (R,R) => R) extends Expr[R] {
    /** Left hand side argument */
    val x:E[R]
    /** Right hand side argument */
    val y:E[R]
  }

  case class Plus[R](x:E[R], y:E[R]) extends Arithmetic[R](_.plus)
  case class Minus[R](x:E[R], y:E[R]) extends Arithmetic[R](_.minus)
  case class Times[R](x:E[R], y:E[R]) extends Arithmetic[R](_.times)
  case class Divide[R](x:E[R], y:E[R]) extends Arithmetic[R](_.div)
  case class Power[R](x:E[R], y:E[R]) extends Arithmetic[R](_.power)

  /** Requires instance of [[Real]] to evaluate */
  abstract class RealExpr[R] extends Expr[R] {
    def eval(real:Real[R], e:Engine[R]):R
  }

  case class Approx[R](x:E[R], y:E[R]) extends RealExpr[Boolean] {
    def eval(real:Real[R], e:Engine[R]):Boolean = real.approx(e(x),e(y))
  }

  case class Num[R](x:R) extends E[R]

  case class IntExpr[R](x:Int) extends Expr[R]
  case class DoubleExpr[R](x:Double) extends Expr[R]
}