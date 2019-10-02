package ds.func

import ds.expr.Expr
import ds.num.Real

/** Variable */
case class Symbol[R](name:String) extends Expr[R]


object Symbol {

  def apply(c:Char) = new Symbol(c.toString)

}



