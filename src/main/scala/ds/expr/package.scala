package ds

import ds.expr.Func.{F1, F2}
import ds.num.Real

package object expr {


  //implicit def exprFunc[T](e:Expr[T]) = Func(e)

  implicit def str2Sym(s:String) = Symbol(s)

  implicit def char2Sym(c:Char) = Symbol(c)


  implicit def F1F1toF2[T1,T2,R](f1f1:F1[T1,F1[T2,R]]):F2[T1,T2,R] = new F2[T1,T2,R] {
    def apply(e:Engine):(T1,T2)=>R = (t1,t2) => f1f1(Expr(t1))(e)(Expr(t2))(e)
    val name:String = f1f1.name
  }

}
