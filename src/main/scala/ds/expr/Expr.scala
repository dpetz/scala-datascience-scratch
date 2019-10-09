package ds.expr

/** Evaluates via  [[Engine]] to result of type ``T``*/
trait Expr[T] {
  def eval(e:Engine):T
  def parts:Seq[Expr[_]]


}

case class Name[T](name:String, expr:Expr[T]) extends Expr[T] {
  def eval(e:Engine):T = e(expr)
  def parts:Seq[Expr[_]]=List(expr)
}

object Expr {


  /* All free [[Symbol]]s recursively
  * @todo Only free symbols
  * @todo order / scopes (lexical vs. dynamic)
  */
  def free(e:Expr[_]):Seq[Symbol[_]] = this match {
    case s:Symbol[_] => List(s)
    case _ => e.parts.flatMap(free(_))
    // @todo resolve Expressibles?
  }

  case class Exception(msg:String, e:Expr[_])
    extends RuntimeException(msg + " in: " + e)

  def apply[T](constant:T):Expr[T]= new Expr[T] {
    def eval(e:Engine):T=constant
    def parts:Seq[Expr[T]]=Nil

  }
}