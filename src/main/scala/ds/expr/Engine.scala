package ds.expr

import ds.expr._
import ds.lina.vec.Vec
import ds.num.real._

trait Engine[R] {
  def apply(e:Expr[R]): R
  def apply(e:Vec[R]): Seq[R]
  def real:Real[R]
}

