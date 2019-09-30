package ds.expr


trait Composed[T] extends Expr[T] {
  def expression(e:Engine):Expr[T]
}

/** Composed of other expressions */
case class AbstractComposed[T](expr:Engine=>Expr[T]) extends Composed[T] {
  def expression(e:Engine):Expr[T] = e(expr)
  def apply(e:Engine):T = e(expr)
}