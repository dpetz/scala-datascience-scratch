package ds.expr

import ds.num.Real

/** Evaluates via  [[Engine]] to result of type ``T``*/
trait Expr[+T]

trait Inputs extends Expr[_] {
  def inputs:Seq[Expr[_]]
}

/** Expression without free parameters */
trait Closed[+T] extends Expr[T] {
  def eval(e:Engine):T
}

object Expr {


  /* All free [[Symbol]]s recursively
  * @todo Only free symbols
  * @todo order / scopes (lexical vs. dynamic)
  */
  def free(e:Expr[_]):Seq[Symbol[_]] = e match {
    case s:Symbol[_] => List(s)
    case i:Inputs => i.inputs.flatMap(free(_))
    // @todo resolve Expressibles?
  }

  case class Exception(msg:String, e:Expr[_])
    extends RuntimeException(msg + " in: " + e)

  def apply[T](constant:T):Expr[T]= new Expr[T] {
    def eval(e:Engine):T=constant
    def inputs:Seq[Expr[T]]=Nil

  }
}