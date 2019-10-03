package ds.expr

trait Func[+R] extends Expr[R] {
  def name:String
  def parts:Seq[Expr[_]] = Nil
}


object Func {


  trait F1[-T,+R] extends Func[T=>R] {
    def apply(x:Expr[T]):Closure.C1[T,R] = Closure.C1(this,x)

  }

  trait F2[-T1,-T2,+R] extends Func[(T1,T2)=>R] {
    def apply(x:Expr[T1], y:Expr[T2]):Closure.C2[T1,T2,R] = Closure.C2(this,x,y)
  }

  trait F3[-T1,-T2,-T3,+R] extends Func[(T1,T2,T3)=>R] {
    def apply(x1:Expr[T1], x2:Expr[T2], x3:Expr[T3]):Closure.C3[T1,T2,T3,R] = Closure.C3(this,x1,x2,x3)
  }


  def apply[T,R](fName:String, func:T=>R):F1[T,R] = new F1[T,R] {
    def apply(e:Engine):T=>R = func
    val name:String = fName
  }

  def apply[T1,T2,R](fName:String, func:(T1,T2)=>R):F2[T1,T2,R] = new F2[T1,T2,R] {
    def apply(e:Engine):(T1,T2)=>R = func
    val name:String = fName
  }

  def apply[T1,T2,T3,R](fName:String, func:(T1,T2,T3)=>R):F3[T1,T2,T3,R] = new F3[T1,T2,T3,R] {
    def apply(e:Engine):(T1,T2,T3)=>R = func
    val name:String = fName
  }


}