package ds

import ds.expr.Expr.{Term, Times}
import ds.expr.{Engine, Expr}
import ds.num.Real

package object vec {

  type E[R] = Expr[R]
  type S[R] = Seq[R]

  def elementwise[R](f:(R,R)=>R): (S[R],S[R])=>S[R] =
    (v:Seq[R],w:Seq[R]) => (v zip w) map (x => f(x._1,x._2))

  def sum[R](v:E[S[R]])(implicit real:Real[R]):E[R] =
    Term("sum",v) { e=> e(v).foldLeft(real.zero)(real.plus)}

  def dot[R:Real](v:E[S[R]],w:E[S[R]]): E[R] = sum( v * w )

  def norm[R](p:Expr[R], v:E[S[R]])(implicit real:Real[R]):Expr[R] =  p match {
    case real.one =>
      v.each(ds.num.abs).all(sum)
    case p:_ =>
      def pow_p = (x:E[R]) => x ** p
      v.each(pow_p).all(sum) ** ( real(1) / p )
  }

  implicit def vecTimes[R](implicit real:Real[R]): Times[S[R],S[R],S[R]] =
    (v, w) => elementwise(real.times)(v, w)


  implicit class SeqExpr[X](val expr:E[S[X]]) {
    /** map */
    def each[Y](f:E[Y]=>E[Y]):E[S[Y]] =
      Term(expr,f) { e => e(expr) map (x => e(f(x))) }
    /** reduce */
    def all[Y](f:E[Seq[X]]=>E[Y]):E[Y] = f(expr)

  }



}
