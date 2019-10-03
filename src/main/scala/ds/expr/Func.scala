package ds.expr

trait Func[+R] extends Expr[R] {
  def name:String
}


object Func {

  case class F2[-T1,-T2,+R](name:String, f:Engine => (T1,T2)=>R) extends Func[R] {
    def apply(x:Expr[T1], y:Expr[T2]):Closure.C2[T1,T2,R] = Closure.C2(this,x,y)
    def parts:Seq[Expr[_]] = Nil
  }

  case class F1[-T,+R](name:String, f:Engine => T=>R) extends Func[R] {
    def apply(x:Expr[T]):Closure.C1[T,R] = Closure.C1(this,x)
    def parts:Seq[Expr[_]] = Nil
  }



}