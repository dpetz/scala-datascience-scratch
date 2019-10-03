package ds

import ds.func.F1
import ds.num.Real

package object expr {


  implicit def exprFunc[T](e:Expr[T]) = F1(e)

  implicit def str2Sym(s:String) = Symbol(s)

  implicit def char2Sym(c:Char) = Symbol(c)

}
