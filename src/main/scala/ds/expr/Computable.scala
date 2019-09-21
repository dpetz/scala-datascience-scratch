package ds.expr


trait Computable[R] extends Expr[R] {
  def compute(e:Engine[R]):R
}