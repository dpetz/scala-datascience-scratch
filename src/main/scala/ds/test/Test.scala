package ds.test

import ds.num.Real

object Test {


  type E[T] = Expr[T]
  type S[T] = Seq[T]

  trait Engine {
    def apply[T](e:E[T]):T
  }

  /** Monadic with ``expr`` as unit and  ``andThen`` as flatMap */
  case class Expr[X](inputs:Product)(eval:Engine=>X) {

    def **(y:E[X])(implicit op:Power[X]):E[X] = lift(op)(this,y)

    def *[Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = lift(op)(this,y)

    def /[Y,Z](y:E[Y])(implicit op:Div[X,Y,Z]):E[Z] = lift(op)(this,y)

    /** flatMap */
    def andThen[Y](f:X=>E[Y]): E[Y] = Expr(this,f) {
      e => e(f(e(this)))
    }

  }

  implicit def expr[T](t:T): E[T] = Expr(None) { _ => t }

  trait Plus[X,Y, Z] extends ((X,Y)=>Z)
  implicit def realPlus[R](implicit real:Real[R]): Plus[R, R, R] = real.plus

  trait Div[X,Y,Z] extends ((X,Y)=>Z)
  implicit def realDiv[R](implicit real:Real[R]): Div[R, R, R] = real.div

  trait Power[R] extends ((R,R)=>R)
  implicit def realPower[R](implicit real:Real[R]):Power[R] = real.power

  trait Times[X,Y,Z] extends ((X,Y)=>Z)
  implicit def vecTimes[R](implicit real:Real[R]): Times[S[R],S[R],S[R]] =
    (v: S[R], w: S[R]) => elementwise[R](real.times)(v, w)


  def elementwise[R](f:(R,R)=>R): (S[R],S[R])=>S[R] =
    (v:Seq[R],w:Seq[R]) => (v zip w) map (x => f(x._1,x._2))

  def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
    (e1, e2) => Expr(e1,e2) {e => f(e(e1), e(e2)) }

  def lift[X,Y](f:X=>Y):E[X]=>E[Y] =
    e1 => Expr(e1) {e => f(e(e1)) }

  def sum[R](v:E[Seq[R]])(implicit real:Real[R]):E[R] =
    Expr(v) { e=> e(v).foldLeft(real.zero)(real.plus) }

  def map[R,S](v:E[Seq[R]],f:R=>Expr[S]):E[Seq[S]] =
    Expr(v,f) { e => e(e(v) map f) }

  def dot[R:Real](v:E[Seq[R]]): E[R] = sum( v * v )

  def norm[R](p:Int)(v:E[Seq[R]])(implicit real:Real[R]):Expr[R] =  p match {
    case real.one =>  sum (map(v, real.abs))
    case p:_ => sum( map(v, expr(_) ** real(p) )) ** real.one./(real(p))

  }




}
