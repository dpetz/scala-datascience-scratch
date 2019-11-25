package ds.expr

import ds.expr.Infix._
import ds.expr.Functions._


/** Evaluates via  [[Engine]] to result of type ``X``.
 * Monadic with ``expr`` as unit and  ``next`` as flatMap.
  * Operators can be configure via type classes */
sealed trait Expr[X] {

  def eval:Engine=>X

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
  def flatMap[Y](f:X=>Expr[Y]):Expr[Y] = Term("ds.expr.flatMap")(this,f) {
    e => e(f(e(this)))
  }

  /** Shorthand for flatMap */
  //def >> [Y](f:X=>Expr[Y]):Expr[Y] = flatMap(f)

  def map[Y](f:X=>Y):Expr[Y] = ds.expr.Functions.map(this,lift(f))
}

trait Expressible[T] extends Expr[T] {
  def express:E[T]
  def eval:Engine=>T = express.eval

}

/** Evaluates to ``value`` regardless of Engine */
case class Const[T](value: T) extends Expr[T] {
  def eval: Engine => T = _ => value
}

/** Names an existing expression. */
case class Named[T](id:String)(expr:Expr[T]) extends Expr[T] {
  def eval: Engine => T = e => e(expr)
}

/** Identifier that is replaced by ``Engine`` with context specific value at each evaluation. */
case class Symbol[T](id: String) extends Expr[T] {
  def eval: Engine => T = throw new UnsupportedOperationException
  // @todo implement Sym.eval
}

case class Term[T](id:String)(args: Product)(override val eval: Engine => T) extends Expr[T]

// Cannot assume ``List[ Expr[_] ]`` because arguments might include functions (eg. map)

object Expr {
  case class Exception(msg: String, e: Expr[_])
    extends RuntimeException(msg + " in: " + e)

}
