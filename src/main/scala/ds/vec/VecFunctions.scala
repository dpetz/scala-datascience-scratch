package ds.vec

import ds.expr.{Engine, Expr, Func}
import ds.expr.Func.{F1, F2, F3}
import ds.num.Real
import ds.num._

class VecFunctions[R:Real](real:Real[R]) {

  type VecOps = F2[Seq[R],Seq[R],Seq[R]]
  type RealOps = F2[Seq[R],Seq[R],R]
  type ScalarField = F1[Seq[R],R]

  private val rAbs = real.func.Abs
  private val rPower = real.func.Power
  private val rTimes = real.func.Times

  /** Zip vector elements and reduce via function. */
  val Elementwise: F3[(R, R) => R, Seq[R], Seq[R], Seq[R]] =
    Func("Elementwise", (f,v,w) => ( v zip w) map (x => f(x._1, x._2)))

  val Times:VecOps = Elementwise -> rTimes

  /** Map function expression to every vector element. */
  val Map:F2[R=>R,Seq[R],Seq[R]] = Func("map", (f,v) => v map f)

  val Sum:F1[Seq[R],R] = Func("sum", _.fold(real.zero)(real.plus))

  val Dot:RealOps = Times >> Sum

  def Norm(p:Int):ScalarField  = p match {
    case 1 => (Map -> rAbs) >> Sum
    case p:_ => ((Map -> (rPower --> p)) >> Sum) >> (rPower --> (1.0 / p))
  }

  def Size[R](v: Vec[R])(implicit real: Real[R]) extends Expr[R] {

    def apply(e:Engine):R = real(e(v).size)
    def parts = List(v)
  }


}
