package ds.expr

/** Composed of other expressions */
trait Composed[T] extends Expr[T] {
  def expr(e:Engine):Expr[T]
  def apply(e:Engine):T = e(expr)
}