package ds.expr

/** Evaluates via  [[Engine]] to result of type ``T``*/
trait Expr[T] extends (Engine => T) {
  def parts:Seq[Expr[_]]


  /* All free [[Symbol]]s recursively
  * @todo Only free symbols
  * @todo order / scopes (lexical vs. dynamic)
  */
  def free:Seq[Symbol[_]] = this match {
    case s:Symbol[_] => List(s)
    case _ => parts.flatMap(_.free)
    // @todo resolve Expressibles?
  }
}

object Expr {
  case class Exception(msg:String, e:Expr[_])
    extends RuntimeException(msg + " in: " + e)

  def apply[T](constant:T):Expr[T]= new Expr[T] {
    def apply(e:Engine):T=constant
    def parts:Seq[Expr[T]]=Nil

  }
}