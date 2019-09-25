package ds.expr

import ds.lina.{Matrix, Query, Vec}
import ds.num.real._

abstract class Engine[R:Real] {

  def apply(e:Expr[Boolean]): Boolean
  def apply(e:Expr[R]): R
  def apply(e:Vec[R]): Seq[R]
  def apply(e:Matrix[R],q:Query): Seq[Seq[R]]

}

