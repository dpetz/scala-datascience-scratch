package ds

import ds.expr.{Engine, Expr}
import ds.num.Real

package object vec {

  type Vec[R] = Expr[Seq[R]]

  implicit def expr2Vec[R: Real](expr: Expr[Seq[R]]):Vec[R] = new Vec[R] {
    def eval(e:Engine):Seq[R] = e(expr)
    def parts: Seq[Expr[_]] = expr.parts
  }

  implicit def seq2Vec[R: Real](s: Seq[R]):Vec[R] = expr2Vec(Expr(s))



}
