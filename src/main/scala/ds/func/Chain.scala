package ds.func

import ds.expr._

object Chain {

  /** Chain F1 followed by F1 */
  case class F1F1[T1,R1,R2](f:F1[T1,R1],g:F1[R1,R2]) extends F1[T1,R2] {
    def eval[R](e:Engine[R]):T1=>R2 = (x:T1) => g.eval(e,f.eval(e,x))
    def name:String = s"${f.name}->${g.name}"
  }

  /** Chain F2 followed by F1 */
  case class F2F1[T1,T2,R1,R2](f:F2[T1,T2,R1],g:F1[R1,R2]) extends F2[T1,T2,R2] {
    def eval[R](e:Engine[R]):(T1,T2)=>R2 = (x:T1,y:T2) => e(g)(e(f)(x,y))
    def name:String = s"${f.name}->${g.name}"
  }

}
