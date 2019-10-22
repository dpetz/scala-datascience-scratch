package ds.vec

import ds.expr.Infix.{Div, Minus, Plus, Times}
import ds.expr.{Expr, Term}
import ds.num.Real
import ds.expr.Implicits._
import ds.num.Implicits._

object Implicits {

  type S[R] = Seq[R]

  implicit def timesVec[R:Real]: Times[S[R]] = (v,w) => (v zip w)(_ * _)

  implicit def divVec[R:Real]: Div[S[R]] = (v,w) => (v zip w)(_ / _)

  implicit def plusVec[R:Real]: Plus[S[R]] = (v,w) => (v zip w)(_ + _)

  implicit def minusVec[R:Real]: Minus[S[R]] = (v,w) => (v zip w)(_ - _)

  implicit def seq2Vec[T](s:Seq[T]): Vec[T] = Vec(expr(s))

  implicit def vec2Expr[T](v:Vec[T]): Expr[Seq[T]] = v.expr_v

  implicit def expr2Vec[T](e:E[S[T]]): Vec[T] = Vec(e)

  implicit def lift[T](s:Seq[Expr[T]]): Expr[Seq[T]] = Term("ds.vec.lift",s) { e => s.map(e(_)) }

  implicit def convertElements[X,Y](s: Seq[X])(implicit f: X => Y):Seq[Y] = s.map(f)




}
