package ds.expr

import ds.expr.Infix._


/** Evaluates via  [[Engine]] to result of type ``X``.
 * Monadic with ``expr`` as unit and  ``next`` as flatMap */
sealed trait Expr[X] {

  type E[T] = Expr[T]

  def eval:Engine=>X

  def **(y:E[X])(implicit op:TimesTimes[X]):E[X] = op(this,y)

  def *(y:E[X])(implicit op:Times[X]):E[X] = op(this,y)

  def /(y:E[X])(implicit op:Div[X]):E[X] = op(this,y)

  def -(y:E[X])(implicit op:Minus[X]):E[X] = op(this,y)

  def +(y:E[X])(implicit op:Plus[X]):E[X] = op(this,y)

  def unary_-(y:E[X])(implicit op:Negate[X]):E[X] = op(this,y)

  def unary_/(y:E[X])(implicit op:Inverse[X]):E[X] = op(this,y)


  // For Scala for comprehensions
  def flatMap[X](f:X=>E[X]):E[X] = next(this,f)

  def map[Y](f:X=>Y):E[Y] = ds.expr.map(this,lift(f))

}

case class Const[T](value: T) extends Expr[T] {
  def eval: Engine => T = _ => value
}

case class Symbol[T](id: String) extends Expr[T] {
  def eval: Engine => T = throw new UnsupportedOperationException

  // @todo implement Sym.eval
}

case class Term[T](args: Product)(override val eval: Engine => T) extends Expr[T]

// Cannot assume ``List[ Expr[_] ]`` because arguments might include functions (eg. map)
// @todo Good design to have a Product include both function identifier and arguments?



object Expr {
  case class Exception(msg: String, e: Expr[_])
    extends RuntimeException(msg + " in: " + e)

}
