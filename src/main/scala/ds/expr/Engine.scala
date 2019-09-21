package ds.expr

trait Engine[R] {
  def apply(e:Expr[R]): R
}

