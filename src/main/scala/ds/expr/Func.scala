package ds.expr

import ds.expr.Closure.{F2B1, F2B2, F3B1}

/** ``Expr``that evaluates to a function. */
trait Func[R] extends Expr[R] {
  def name:String
  /** Returns ``Nil`` unless overwritten. */
  def parts:Seq[Expr[_]] = Nil
}

/** Defines function expression traits / classes with different numbers of arguments */
object Func {
  
  type E[R] = Expr[R]

  /** ``Expr`` evaluating to a function with one input */
  trait F1[T,R] extends (E[T] => E[R]) with Func[T=>R] {
    def apply(x:E[T]):Closure.C1[T,R] = Closure.C1(this,x)
    /** Evaluate this, then ``f`` */
    def >>[R2](f:F1[R,R2]):F1[T,R2] = F1F1(this,f)
  }

  /** Chain F1 followed by F1 */
  case class F1F1[T1,R1,R2](f:F1[T1,R1],g:F1[R1,R2]) extends F1[T1,R2] {
    def eval(e:Engine):T1=>R2 = (x:T1) => e(g)(e(f)(x))
    def name:String = s"${f.name}->${g.name}"
  }

  /** Chain F2 followed by F1 */
   case class F2F1[T1,T2,R1,R2](f:F2[T1,T2,R1],g:F1[R1,R2]) extends F2[T1,T2,R2] {
    def eval(e:Engine):(T1,T2)=>R2 = (x:T1,y:T2) => e(g)(e(f)(x,y))
    def name:String = s"${f.name}->${g.name}"
  }

  /** ``Expr`` evaluating to a function with two inputs */
  trait F2[T1,T2,R] extends ((E[T1],E[T2]) => E[R]) with Func[(T1,T2)=>R] {
    /** Assign all inputs */
    def apply(x:E[T1], y:E[T2]):Closure.C2[T1,T2,R] = Closure.C2(this,x,y)
    /** Evaluate this, then ``f`` */
    def >>[T](f:F1[R,T]):F2[T2,T2,T] = F2F1(this,f)
    /** Assign first input */
    def ->(x1:E[T1]):F1[T2,R] = F2B1(this,x1)
    /** Assign second input */
    def -->(x2:E[T2]):F1[T1,R] = F2B2(this,x2)
  }

  /** ``Expr`` evaluating to a function with three inputs */
  trait F3[T1,T2,T3,R] extends ((E[T1],E[T2],E[T3]) => E[R]) with Func[(T1,T2,T3)=>R] {
    /** Assign all inputs */
    def apply(x1:E[T1], x2:E[T2], x3:E[T3]):Closure.C3[T1,T2,T3,R] = Closure.C3(this,x1,x2,x3)
    /** Assign first input */
    def ->(x1:E[T1]):F2[T2,T3,R] = F3B1(this,x1)
  }

  /** Name and wrap function of one input as ``Expr`` */
  def apply[T,R](fName:String, func:T=>R):F1[T,R] = new F1[T,R] {
    def eval(e:Engine):T=>R = func
    val name:String = fName
  }

  /** Name and wrap function of two inputs as ``Expr`` */
  def apply[T1,T2,R](fName:String, func:(T1,T2)=>R):F2[T1,T2,R] = new F2[T1,T2,R] {
    def eval(e:Engine):(T1,T2)=>R = func
    val name:String = fName
  }

  /** Name and wrap function of three inputs as ``Expr`` */
  def apply[T1,T2,T3,R](fName:String, func:(T1,T2,T3)=>R):F3[T1,T2,T3,R] = new F3[T1,T2,T3,R] {
    def eval(e:Engine):(T1,T2,T3)=>R = func
    val name:String = fName
  }

}