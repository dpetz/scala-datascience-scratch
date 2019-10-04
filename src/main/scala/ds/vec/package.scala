package ds

import ds.expr.Expr
import ds.num.Real

package object vec {

  type Vec[R] = Expr[Seq[R]]

  implicit def expr2Vec[R: Real](e: Expr[Seq[R]]):VecInfix[R] = new VecInfix[R](e)

  implicit def seq2Vec[R: Real](s: Seq[R]):VecInfix[R] = expr2Vec(Expr(s))

}
