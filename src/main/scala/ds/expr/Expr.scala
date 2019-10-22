package ds.expr

import ds.expr.Infix._


/** Evaluates via  [[Engine]] to result of type ``X``.
 * Monadic with ``expr`` as unit and  ``next`` as flatMap */
sealed trait Expr[X] {

  type E = Expr[X]

  def eval:Engine=>X

  def **(y:E)(implicit op:TimesTimes[X]):E = op(this,y)

  def *(y:E)(implicit op:Times[X]):E = op(this,y)

  def /(y:E)(implicit op:Div[X]):E = op(this,y)

  def -(y:E)(implicit op:Minus[X]):E = op(this,y)

  def +(y:E)(implicit op:Plus[X]):E = op(this,y)

  def ~(y:E)(implicit op:Approx[X]):Expr[Boolean] = op(this,y)


  def unary_-(implicit op:Negate[X]):E = op(this)

  def unary_/(implicit op:Inverse[X]):E = op(this)


  // For Scala for comprehensions
  def flatMap[Y](f:X=>Expr[Y]):Expr[Y] = transform(this)(f)

  def map[Y](f:X=>Y):Expr[Y] = ds.expr.map(this,lift(f))


}

case class Const[T](value: T) extends Expr[T] {
  def eval: Engine => T = _ => value
}

case class Named[T](id:String)(expr:Expr[T]) extends Expr[T] {
  def eval: Engine => T = e => e(expr)
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
