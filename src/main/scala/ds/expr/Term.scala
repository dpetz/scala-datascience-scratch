package ds.expr

/** Either extend with ``case class`` or use ``Term.apply`` methods. */
trait Term[Y] extends Expr[Y] with Product

/** Factory for ``Term``s with function passed in as first argument
  * (named ``f``) and function arguments ``x1``, .., ``xn`` */
object Term {

  case class Term1[X,Y](f:X=>Engine=>Y,x1:X) extends Term[Y] {
    def eval:Engine=>Y = f(x1)
  }

  case class Term2[X1,X2,Y](f:(X1,X2)=>Engine=>Y,x1:X1,x2:X2) extends Term[Y] {
    def eval:Engine=>Y = f(x1,x2)
  }

  /** Function with one argument */
  def apply[X,Y](x1:X)(f:X=>Engine=>Y): Term[Y] = Term1(f,x1)

  /** Function with one argument */
  def apply[X1,X2,Y](x1:X1,x2:X2)(f:(X1,X2)=>Engine=>Y): Term[Y] = Term2(f,x1,x2)


}
