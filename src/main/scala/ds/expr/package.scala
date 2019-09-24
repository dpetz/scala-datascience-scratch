package ds

import ds.num.real._

package object expr {

  abstract class Expr[R]

  type E[R] = Expr[R]


  /** Expression evaluating to a real number. */
  abstract class RealExpr[R](eval:Engine[R] => R)  extends E[R]{

    /** @see Real.power */
    def **(y: E[R]): E[R] = Arithmetic(this,y)(_.power(_,_))
    /** @see Real.plus */
    def +(y: E[R]): E[R] = Arithmetic(this,y)(_.plus(_,_))
    /** @see Real.minus */
    def -(y: E[R]): E[R] = Arithmetic(this,y)(_.minus(_,_))
    /** @see Real.times */
    def *(y: E[R]): E[R] = Arithmetic(this,y)(_.times(_,_))
    /** @see Real.div */
    def /(y: E[R]): E[R] = Arithmetic(this,y)(_.div(_,_))
    /** @see Real.approx */
    def ~(y: E[R]): E[Boolean] = Relation(this,y)(_.approx(_,_))

  }

  /** References [[Real]] function of type `(R,R) => R` such as ``Real.plus` */
  case class Arithmetic[R] (x:E[R],y:E[R])(f:(Real[R],R,R) => R)
    extends RealExpr[R](e => f(e.real,e(x),e(y)))

  /** Expression in two arguments evaluating to a ``Boolean`` */
  case class Relation[R](x:E[R], y:E[R])(f:(Real[R],R,R) => Boolean) extends E[Boolean]

  case class Terminal[R](f:Real[R]=>R) extends RealExpr[R] (e => f(e.real))


  /** Convert ``Ìnt`` to [[Expr]] on the fly */
  implicit def big2Expr[R](x: BigDecimal):E[R] = Terminal(_(x))
  /** Convert ``Ìnt`` to [[Expr]] on the fly */
  implicit def int2Expr[R](x: Int):E[R] = Terminal(_(x))
  /** Convert ``Ìnt`` to [[Expr]] on the fly */
  implicit def dbl2Expr[R](x: Double): E[R] = Terminal(_(x))
  /** Convert ``Ìnt`` to [[Expr]] on the fly */
  implicit def real2Expr[R](x: R): E[R] = Terminal(_ => x)
}