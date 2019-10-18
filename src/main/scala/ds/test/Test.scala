package ds.test

import ds.num.Real

object Test {


  type E[T] = Expr[T]

  type Engine

  /** Monadic with ``Expr.apply`` as unit and  ``chain`` as flatMap */
  case class Expr[X](eval:Engine=>X) {
    //def eval(e:Engine):X
    //def +[Y,Z](y:Expr[Y])(implicit op:Plus[X,Y,Z]):Expr[Z] = op(this,y)

  }

  object Expr {
    def apply[T](t:T):E[T] =  Expr {
      _ => t
    }
  }


  implicit class Function2Expr[X1,X2,Y](f1:(X1,X2)=>Z) {
    def °[Z](g:Y=>Z):Chain(f,g)
  }

  case class Chain[X,Y,Z] extends (X=>)


  trait Plus[X,Y] extends ((X,Y)=>X)

  trait Times[X,Y] extends ((X,Y)=>X)

  def realPlus[R](implicit real:Real[R]): Plus[R, R, R] = real.plus _

  def vecTimes[R](implicit real:Real[R]): Times[Seq[R], Seq[R]] =
    (v:Seq[R], w:Seq[R]) => (v zip w) map (x => real.plus(x._1,x._2))

  def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
    (e1, e2) => Expr(f(e1.eval, e2.eval))


  def sum[R](implicit real:Real[R]): (Seq[R] => R) =
    (v:Seq[R]) => v.foldLeft(real.zero)(real.plus)

  def dot[R:Real](v:E[Seq[R]]): E[R] =
    vecTimes(v,v)


  def addMatrices(v:Expr[])

  trait Matrix extends


  trait Monad[Expr[_]]  {

    def unit[R](x:()=>R):Expr[R]

    def flatMap[R,S](e:Expr[R])(f:R=>Expr[S]):Expr[S]

  }


}
