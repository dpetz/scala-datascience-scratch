package ds.lina

import ds.expr._
import ds.num.real._
import ds.lina.vec._

case class Norm[R:Real](v:Vec[R],p:Expr[R]) extends RealValued[R] (e =>
  e(p) match {
    case 1 => v map {_.abs} sum
    case p_ => ( v map { _ ** p_} sum ) ** ( 1.0 / p_ )
  })
