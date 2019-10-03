package ds.vec

import ds.expr.Func.F3
import ds.expr.Engine
import ds.num.Real

class Elementwise[R:Real] extends F3[Seq[R],Seq[R],(R,R)=>R,Seq[R]] {

  def apply(e:Engine): (Seq[R], Seq[R], (R, R) => R) => Seq[R] =
    (v:Seq[R],w:Seq[R],f:(R,R)=>R) =>( v zip w) map (x => f(x._1, x._2))

  val name:String = "Elementwise"

}


