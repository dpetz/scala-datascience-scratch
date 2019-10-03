package ds

import ds.expr.{Composed, Engine, Expr}
import ds.num.Real

package object func {



  implicit def func[R:Real](f:R=>Expr[R]): Func[R] = Func(f)

  implicit class Assign[T](x:T) extends {
    def >>(f:Func[T])(implicit e:Engine):T = e.assign(f.x,x)(f)
  }


  import ds.vec.Vec

  /** https://en.wikipedia.org/wiki/Scalar_field */
  class ScalarField[R:Real](f:Seq[R]=>R) extends RealValuedFunc[Seq[R],R](f)
  /** https://en.wikipedia.org/wiki/Vector_field */
  trait VectorField[R] extends Expr[Seq[R]=>Seq[R]]



  /** Function of one argument evaluating to Real */
  implicit class RealValuedFunc[X,R](f:X=>R)(implicit real:Real[R]) extends Expr[X=>R] {

    def apply(e:Engine):(X=>R) = f

    //def -(y:R):RealFunc[A,R] => R = { x: X => real.minus(f(x), y) }

    //def /(y:R):X => R = { x: X => real.div(f(x), y) }

    /** Negate real function (one argument) */
    def unary_- : Expr[X=>R]= Negate(expr(f))

  }



  implicit def expr[X,T](f:X=>T):Expr[X=>T]= Func(f)

  case class Negate[X,R](f:Expr[X=>R])(implicit r:Real[R])
    extends Composed[X=>R] ( _ =>  Then[X,R,R](f,expr(r.negate)) )

  case class Then[X,A,B](f:Expr[(X=>A)], g:Expr[(A=>B)]) extends Expr[(X=>B)]{
    def apply(e:Engine):X=>B = { x:X => e(g)(e(f)(x)) }
  }

  implicit class VecValuedFunction[X, R](f: X => Seq[R])(implicit real:Real[R])
    extends Expr[X=>Seq[R]] {

    def apply(e:Engine): X=>Seq[R]  = f

    /** Negate real function (one argument) */
    def unary_- : Expr[X=>Vec[R]] = Then(f, real.negate)

  }

  /*
 implicit class FunctionOps[A, B](f: A => B) {

   def recode(before: B, after: B): A => B = { x: A =>
     val y = f(x)
     if (y == before) after else y
   }
 }
 */

}
