package ds.test

import ds.num.Real
import ds.test
import ds.test.Test.E

object Test {


  type E[T] = Expr[T]

  trait Engine {
    def scalar[T](e:E[T]):T
    def vec[T](e:E[T]):Seq[T]
  }

  /** Monadic with ``Expr.apply`` as unit and  ``andThen`` as flatMap */
  case class Expr[T](eval:Engine=>Seq[T]) {
    //def eval(e:Engine):X
    def **(y:E[T])(implicit op:Power[T]):E[T] = map(op(_,y))

    def *[X,Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = op(this,y)

    def /[X,Y,Z](y:E[Y])(implicit op:Div[X,Y,Z]):E[Z] = op(this,y)

    /** flatMap */
    def andThen[S](f:T=>E[S]): E[S] = Expr {
      e => e(f(e(this)))
    }

    def map[S](f:E[T]=>E[S]):E[T]
    def map[S](f:T=>S):E[T]


  }

  /*
  object Expr {
    def const[T](t:T):E[T] = expr(t)
  }
   */

  /** Sequence S of Elements X  */
  trait Vec[T,X]

  trait Power[R] extends ((R,R)=>R)

  implicit def realPower[R](implicit real:Real[R]):Power[R] = real.power

    // (x,p) => Expr { e => e.vec(x.liftmap(real.power(_,e.scalar(p)))) }

  //implicit def expr[T](t:T): E[T] = Expr { _ => List(t) }



  trait Plus[X,Y] extends ((X,Y)=>X)

  trait Div[X,Y,Z] extends ((E[X],E[Y])=>E[Z])

  trait Times[X,Y,Z] extends ((E[X],E[Y])=>E[Z])

  implicit def realPlus[R](implicit real:Real[R]): Plus[R, R] = real.plus _

  implicit def realDiv[R](implicit real:Real[R]): Div[R, R, R] = (x,p) => Expr { e => real.div(e(x),e(p)) }

  def elementwiseTimes[R](implicit real:Real[R]): (Seq[R],Seq[R])=>Seq[R] =
    (v:Seq[R],w:Seq[R]) => (v zip w) map (x => real.plus(x._1,x._2))

  def lift[X1,X2,Y](f:(X1,X2)=>Y):(E[X1],E[X2])=>E[Y] =
    (e1, e2) => Expr {e => List(f(e.scalar(e1), e.scalar(e2))) }

  def lift[X,Y](f:X=>Y):E[X]=>E[Y] =
    (e1) => Expr {e => List(f(e.scalar(e1))) }

  //def :*:[R:Real](v:E[Seq[R]], w:E[Seq[R]]):E[Seq[R]] = lift(elementwiseTimes)(v,w)

  def sum[R](v:E[Seq[R]])(implicit real:Real[R]):E[R] =
    Expr { e=> e(v).foldLeft(real.zero)(real.plus) }

  /*
  def map[R,S](v:E[Seq[R]],f:R=>Expr[S]):E[Seq[S]] =
    Expr { e => e(e(v) map f) }
*/

  def dot[R:Real](v:E[Seq[R]]): E[R] = sum(v*v)



  /** V will be a Seq[R:Real] */
  /*
  abstract class Vec[R,V <: Seq[R]] {
    def map(v:V,f:)
  }
*/

  def norm[R](p:Int)(v:E[Seq[R]])(implicit real:Real[R]):Expr[R] =  p match {
    case real.one => sum (v.map(real.abs) )
    case p:_ => sum( v.map( x => expr(x) ** real(p) )) ** real.one./(real(p))

  }




}
