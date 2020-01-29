package ds.expr

import ds.expr.Infix._


/** Evaluates to ``value`` regardless of Engine */
case class Const[T](value: T) extends Expr[T] {
 def eval(e:Engine) : T = value
}

/** Reifies another expression */
trait Expressible[T] extends Expr[T] {
 def express:Expr[T]
 def eval(e:Engine):T = express.eval(e)

}

/** Application of a function to the arguments of the Product.
  * Subclasses are usually case classes.  */
trait Term[Y] extends Expr[Y] {
  def func:E[_]
  def args:Product

}

object Term {

 def apply[Y,X1,X2](x1: E[X1], x2: E[X2])(f:E[(X1,X2) => Y]):Term2[Y,X1,X2]=Term2(f)(x1,x2)
}

case class Term1[Y,X1](f:E[X1 => Y])(x1: E[X1]) extends Term[Y] {
 def eval(e:Engine):Y = e(f)(e(x1))
 def args:List[ Expr[_] ] = List(x1)
 def func:E[X1 => Y] = f
}

case class Term2[Y,X1,X2](func:E[(X1, X2) => Y], args:Product2[E[X1],E[X2]]) extends Term[Y] {
 def eval(e:Engine):Y = e(func)(e(args._1), e(args._2))
}

/** Identifier that is replaced by ``Engine`` with context specific value at each evaluation. */
case class Symbol[T](id: String) extends Expr[T] {
 def eval(e: Engine) = throw new UnsupportedOperationException
 // @todo implement Sym.eval
}

/** Evaluates via  [[Engine]] to result of type ``X``.
 * Monadic with ``expr`` as unit and  ``next`` as flatMap.
  * Operators can be configure via type classes */
 sealed trait Expr[X] {

  def eval(e:Engine):X


 def named(s:String):Tag[String,X] = Tag(s,this)

  def **(y:Expr[X])(implicit op:TimesTimes[X]):Expr[X] = op(this,y)
  // @todo create term instead executing immediately

  def *(y:Expr[X])(implicit op:Times[X]):Expr[X] = op(this,y)

  def /(y:Expr[X])(implicit op:Div[X]):Expr[X] = op(this,y)

  def -(y:Expr[X])(implicit op:Minus[X]):Expr[X] = op(this,y)

  def +(y:Expr[X])(implicit op:Plus[X]):Expr[X] = op(this,y)

  def ~(y:Expr[X])(implicit op:Approx[X]):Expr[Boolean] = op(this,y)

  def <(y:Expr[X])(implicit op:Compare[X]):Expr[Boolean] = op(this,y).map(_ < 0)

  def <=(y:Expr[X])(implicit op:Compare[X]):Expr[Boolean] = op(this,y).map(_ <= 0)

  def ==(y:Expr[X])(implicit op:Compare[X]):Expr[Boolean] = op(this,y).map(_ = 0)

  def >=(y:Expr[X])(implicit op:Compare[X]):Expr[Boolean] = op(this,y).map(_ >= 0)

  def >(y:Expr[X])(implicit op:Compare[X]):Expr[Boolean] = op(this,y).map(_ > 0)

  def unary_-(implicit op:Negate[X]):Expr[X] = op(this)

  // For Scala for comprehensions
  def flatMap[Y](f:X=>Expr[Y]):Expr[Y] = Term(f,this) {
    e => e(f(e(this)))
  }

  /** Shorthand for flatMap */
  //def >> [Y](f:X=>Expr[Y]):Expr[Y] = flatMap(f)

  def map[Y](f:X=>Y):Expr[Y] = Named("ds.expr.map", Term(this) {
   x => e => e(f(e(x)))
  })


// Cannot assume ``List[ Expr[_] ]`` because arguments might include functions (eg. map)
/*
object Expr {
  case class Exception(msg: String, e: Expr[_])
    extends RuntimeException(msg + " in: " + e)

}
*/