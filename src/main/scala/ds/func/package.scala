package ds

import ds.expr.{Engine, Expr}

package object func {

  implicit def F1F1toF2[X1,X2,Y](f1f1:F1[X1,F1[X2,Y]]):F2[X1,X2,Y] = new F2[X1,X2,Y] {
    def eval[R](e:Engine[R],x1:Expr[X1],x2:Expr[X2]):Y = f1f1.eval(e,x1).eval(e,x2)
    val name:String = f1f1.name
  }

}
