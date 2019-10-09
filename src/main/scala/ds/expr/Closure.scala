package ds.expr

import ds.expr.Func.{F1, F2, F3}

trait Closure[R] extends Expr[R]

object Closure {

  /** Binds function of on argument to its input. */
  case class C1[T1,R](func:F1[T1,R], x:Expr[T1]) extends Closure[R] {
    def eval(e:Engine):R=e(func)(e(x))
    def parts = List(func,x)
  }

  /** Binds function of two arguments to its input. */
  case class C2[T1,T2,R](func:F2[T1,T2,R], x:Expr[T1], y:Expr[T2]) extends Closure[R] {
    def eval(e:Engine):R=e(func)(e(x),e(y))
    def parts = List(func,x,y)
  }

  /** Binds function of three arguments to its input. */
  case class C3[T1,T2,T3,R](func:F3[T1,T2,T3,R], x1:Expr[T1], x2:Expr[T2], x3:Expr[T3]) extends Closure[R] {
    def eval(e:Engine):R=e(func)(e(x1),e(x2),e(x3))
    def parts = List(func,x1,x2,x3)
  }


  /** Bind first input of an [[F3]] */
  case class F3B1[T1,T2,T3,R](func:F3[T1,T2,T3,R], x1:Expr[T1]) extends F2[T2,T3,R] with Closure[(T2,T3)=>R] {
    def name:String = s"$func[$x1]"
    def eval(e:Engine):(T2,T3)=>R= (x2,x3) => e(func)(e(x1),x2,x3)
    override def parts = List(func,x1)
  }

  /** Bind first input of an [[F2]] */
  case class F2B1[T1,T2,T3,R](func:F2[T1,T2,R], x1:Expr[T1]) extends F1[T2,R] with Closure[(T2)=>R] {
    def name:String = s"$func[$x1,]"
    def eval(e:Engine):T2=>R= x2 => e(func)(e(x1),x2)
    override def parts = List(func,x1)
  }

  /** Bind second input of an [[F2]] */
  case class F2B2[T1,T2,T3,R](func:F2[T1,T2,R], x2:Expr[T2]) extends F1[T1,R] with Closure[(T1)=>R] {
    def name:String = s"$func[,$x2]"
    def eval(e:Engine):T2=>R= x1 => e(func)(x1,e(x2))
    override def parts = List(func,x2)
  }



}