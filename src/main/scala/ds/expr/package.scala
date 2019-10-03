package ds

import ds.func.Func
import ds.num.Real

package object expr {


  implicit def exprFunc[R:Real](e:Expr[R]) = Func(e)

}
