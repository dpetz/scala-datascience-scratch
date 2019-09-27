package ds.calc

import ds.expr.{Engine, Expr}
import ds.lina.Vec
import ds.lina.Vec._
import ds.num.real._


// https://www.scala-lang.org/api/current/scala/math/index.html
// https://www.scala-lang.org/api/current/scala/Double$.html
// https://www.scala-lang.org/api/current/scala/math/Numeric.html
// https://www.scala-lang.org/api/current/scala/math/Ordered.html

 object Func {

   import ds.lina.Vec

   /** https://en.wikipedia.org/wiki/Scalar_field */
   class ScalarField[R:Real](f:Seq[R]=>R) extends RealValuedFunc[Seq[R],R](f)
   /** https://en.wikipedia.org/wiki/Vector_field */
   trait VectorField[R] extends Expr[R,(Seq[R]=>Seq[R])]


  /** Function of one argument evaluating to Real */
  implicit class RealValuedFunc[X,R](f:X=>R)(implicit real:Real[R]) extends Expr[R,(X=>R)] {

    def apply(e:Engine[R]):(X=>R) = f

    //def -(y:R):RealFunc[A,R] => R = { x: X => real.minus(f(x), y) }

    //def /(y:R):X => R = { x: X => real.div(f(x), y) }

    /** Negate real function (one argument) */
    def unary_- : RealValuedFunc[X,R] = -f(_)

  }

   implicit class VecValuedFunction[X, R](f: X => Seq[R])(implicit real:Real[R]) {

     /** Negate real function (one argument) */
     def unary_- : VecValuedFunction[X,R] = -f(_)

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


