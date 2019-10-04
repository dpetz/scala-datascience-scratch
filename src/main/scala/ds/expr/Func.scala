package ds.expr

/** ``Expr``that evaluates to a function. */
trait Func[+R] extends Expr[R] {
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
  }

  /** ``Expr`` evaluating to a function with two inputs */
  trait F2[T1,T2,R] extends ((E[T1],E[T2]) => E[R]) with Func[(T1,T2)=>R] {
    def apply(x:E[T1], y:E[T2]):Closure.C2[T1,T2,R] = Closure.C2(this,x,y)
  }

  /** ``Expr`` evaluating to a function with three inputs */
  trait F3[T1,T2,T3,R] extends ((E[T1],E[T2],E[T3]) => E[R]) with Func[(T1,T2,T3)=>R] {
    def apply(x1:E[T1], x2:E[T2], x3:E[T3]):Closure.C3[T1,T2,T3,R] = Closure.C3(this,x1,x2,x3)
  }

  /** Name and wrap function of one input as ``Expr`` */
  def apply[T,R](fName:String, func:T=>R):F1[T,R] = new F1[T,R] {
    def apply(e:Engine):T=>R = func
    val name:String = fName
  }

  /** Name and wrap function of two inputs as ``Expr`` */
  def apply[T1,T2,R](fName:String, func:(T1,T2)=>R):F2[T1,T2,R] = new F2[T1,T2,R] {
    def apply(e:Engine):(T1,T2)=>R = func
    val name:String = fName
  }

  /** Name and wrap function of three inputs as ``Expr`` */
  def apply[T1,T2,T3,R](fName:String, func:(T1,T2,T3)=>R):F3[T1,T2,T3,R] = new F3[T1,T2,T3,R] {
    def apply(e:Engine):(T1,T2,T3)=>R = func
    val name:String = fName
  }

}