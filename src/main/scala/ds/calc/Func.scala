package ds.calc

import ds.expr.{Composed, Engine, Expr}
import ds.vec
import ds.num.Real
import ds.calc.Func._


// https://www.scala-lang.org/api/current/scala/math/index.html
// https://www.scala-lang.org/api/current/scala/Double$.html
// https://www.scala-lang.org/api/current/scala/math/Numeric.html
// https://www.scala-lang.org/api/current/scala/math/Ordered.html


 object Func {

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

   def apply[X,T](f:X=>T):Expr[X=>T]= (e: Engine) => f

   implicit def expr[X,T](f:X=>T):Expr[X=>T]= apply(f)

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


