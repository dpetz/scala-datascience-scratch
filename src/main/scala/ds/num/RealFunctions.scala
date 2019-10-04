package ds.num

import ds.expr.Func
import ds.expr.Func.{F1, F2}

class RealFunctions[R](val real:Real[R]) {

  val Plus   : F2[R, R, R]       = Func("+", real.plus)
  val Minus  : F2[R, R, R]       = Func("-", real.minus)
  val Times  : F2[R, R, R]       = Func("*", real.times)
  val Div    : F2[R, R, R]       = Func("/", real.div)
  val Approx : F2[R, R, Boolean] = Func("~", real.approx)
  val Power  : F2[R, R, R]       = Func("**",real.power)
  val Abs    : F1[R, R]          = Func("+", real.abs)

}
