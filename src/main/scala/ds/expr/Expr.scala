package ds.expr

import ds.expr.Expr._
import ds.matrix.Matrix
import ds.num.Real
import ds.matrix._



/** Evaluates via  [[Engine]] to result of type ``X``.
 * Monadic with ``expr`` as unit and  ``next`` as flatMap */
sealed trait Expr[X] {

  type E[T] = Expr[T]

  def eval:Engine=>X

  def **[Y,Z](y:E[Y])(implicit op:TimesTimes[X,Y,Z]):E[Z] = op(this,y)

  def *[Y,Z](y:E[Y])(implicit op:Times[X,Y,Z]):E[Z] = lift(op)(this,y)

  def /[Y,Z](y:E[Y])(implicit op:Div[X,Y,Z]):E[Z] = lift(op)(this,y)

  // For Scala for comprehensions
  def flatMap[Y](f:X=>E[Y]):E[Y] = next(this,f)

  def map[Y](f:X=>Y):E[Y] = ds.expr.map(this,lift(f))

}

case class Const[T](value: T) extends E[T] {
  def eval: Engine => T = _ => value
}

case class Symbol[T](id: String) extends E[T] {
  def eval: Engine => T = throw new UnsupportedOperationException

  // @todo implement Sym.eval
}

case class Term[T](args: Product)(override val eval: Engine => T) extends E[T]

// Cannot assume ``List[ Expr[_] ]`` because arguments might include functions (eg. map)
// @todo Good design to have a Product include both function identifier and arguments?



object Expr {

  type E[T] = Expr[T]


  /** Create Term by finding and bind evaluation code for ``args``.
    * */
  def fromArgs[R](args: Product)(implicit real: Real[R]): E[_] = {

    val term = List(args)
    val func = term.head
    val vars = term.tail

    func.asInstanceOf[String] match {
      case "ds.matrix.timesMatrix" => timesMatrix(real)(
        vars.head.asInstanceOf[E[Matrix[R]]], vars(1).asInstanceOf[E[Matrix[R]]])
    }
    // @todo work for new identifiers using reflection
    // (https://docs.scala-lang.org/overviews/reflection/overview.html)

  }

  case class Exception(msg: String, e: Expr[_])
    extends RuntimeException(msg + " in: " + e)




}
