package ds.func

import ds.expr.{Engine, Expr}

object Bind {




  /** Bind first input of an [[F2]] */
  case class F2B1[X1,X2,X3,Y](func:F2[X1,X2,Y], x1:Expr[X1]) extends F1[X2,Y] with Closure[(X2)=>Y] {
    def name:String = s"$func[$x1,]"
    def eval(e:Engine[R]):X2=>Y= x2 => e(func)(e(x1),x2)
    override def parts = List(func,x1)
  }

  /** Bind second input of an [[F2]] */
  case class F2B2[X1,X2,X3,Y](func:F2[X1,X2,Y], x2:Expr[X2]) extends F1[X1,Y] with Closure[(X1)=>Y] {
    def name:String = s"$func[,$x2]"
    def eval(e:Engine[R]):X2=>Y= x1 => e(func)(x1,e(x2))
    override def parts = List(func,x2)
  }

}
