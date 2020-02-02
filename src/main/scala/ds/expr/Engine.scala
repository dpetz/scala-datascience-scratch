package ds.expr

import ds.matrix.Matrix
import ds.num.Real
import ds.matrix.Implicits._
import ds.expr.Symbol


/** Engine for `Real` arithmetic. Map is immutable.*/
 class Engine(val symbols:Map[Symbol[_],_] = Map.empty) {


  // @todo cache expensive computations
  def apply[A](expr: Expr[A]): A = expr match {
    case s: Symbol[A] => symbols(s).asInstanceOf[A]
    case e => e.eval(this)
  }

  def bind[X](s:Symbol[X],x:X) : Engine = new Engine(symbols + (s -> x))

  /** Create Term by finding and bind evaluation code for ``args``.
    * */
  def fromArgs[R](args: Product)(implicit real: Real[R]): Expr[_] = {

    val term = List(args)
    val func = term.head
    val vars = term.tail

    func.asInstanceOf[String] match {
      case "ds.matrix.timesMatrix" => times(real)(
        vars.head.asInstanceOf[Expr[Matrix[R]]], vars(1).asInstanceOf[Expr[Matrix[R]]]
      )
    }
    // @todo work for new identifiers using reflection
    // (https://docs.scala-lang.org/overviews/reflection/overview.html)


  }
}

object Engine {

  case class Exception[R](eng:Engine, expr:Expr[R]) extends RuntimeException {
    override def toString = s"Engine $eng cannot evaluate $expr"
  }

}


