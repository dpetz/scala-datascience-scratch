package ds.expr

/** Names an existing expression. */
case class Name[T](id:String, expr:Expr[T]) extends Expr[T] {
  def eval: Engine => T = e => e(expr)
}
