package ds.expr

/** Evaluates via  [[Engine]] to result of type ``T``*/
trait Expr[T] extends (Engine => T)


