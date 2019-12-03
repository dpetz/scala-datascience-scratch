package ds.expr

/** Identifier that is replaced by ``Engine`` with context specific value at each evaluation. */
case class Symbol[T](id: String) extends Expr[T] {
  def eval: Engine => T = throw new UnsupportedOperationException
  // @todo implement Sym.eval
}
