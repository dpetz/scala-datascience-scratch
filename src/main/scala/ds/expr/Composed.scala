package ds.expr

/** Composed of other expressions */
trait Expressible[T] extends Expr[T] {
  def express(e:Engine):Expr[T]
  def apply(e:Engine):T = e(express(e))
}