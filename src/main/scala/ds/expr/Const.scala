package ds.expr

/** Evaluates to ``value`` regardless of Engine */
case class Const[T](value: T) extends Expr[T] {
  def eval: Engine => T = _ => value
}
