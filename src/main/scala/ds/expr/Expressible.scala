package ds.expr

/** Composed of other expressions */
trait Expressible[+T] extends Expr[T] {
  val express:Expr[T]
  def eval(e:Engine):T = e(express)
  def parts = List(express)
}