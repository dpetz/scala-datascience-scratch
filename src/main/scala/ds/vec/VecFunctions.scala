package ds.vec

import ds.expr.Func
import ds.expr.Func.{F1, F2, F3}
import ds.num.Real
import ds.expr._

class VecFunctions[R](real:Real[R]) {

  type VecOps = F2[Seq[R],Seq[R],Seq[R]]

  /** Zip vector elements and reduce via function. */
  val Elementwise: F3[(R, R) => R, Seq[R], Seq[R], Seq[R]] =
    Func("Elementwise", (f,v,w) => ( v zip w) map (x => f(x._1, x._2)))

  val Times:VecOps = Func("*", F1F1toF2(Func("test",Elementwise.curried(real.func.Times))))

  /** Map function expression to every vector element. */
  val Map:F2[Seq[R],R=>R,Seq[R]] = Func("map", _ map _)

  val Sum:F1[Seq[R],R] = Func("sum", _.fold(real.zero)(real.plus))

  val Dot:F2[Seq[R],Seq[R],R] = Times


}
