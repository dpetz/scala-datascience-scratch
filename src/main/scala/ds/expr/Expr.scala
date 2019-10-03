package ds.expr

import ds.func.Symbol

/** Evaluates via  [[Engine]] to result of type ``T``*/
trait Expr[T] extends (Engine => T) {
  def inputs:Seq[Expr[_]]


  /* All free [[Symbol]]s recursively
  * @todo Only free symbols
  * @todo order / scopes (lexical vs. dynamic)
  */
  def free:Seq[Symbol[_]] = this match {
    case s:Symbol[_] => List(s)
    case _ => inputs.flatMap(_.free)
    // @todo resolve Expressibles?
  }
}