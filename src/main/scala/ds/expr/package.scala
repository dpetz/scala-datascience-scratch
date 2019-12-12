package ds


package object expr {

  type Scalar[R] = Expr[R]

  implicit def str2Sym[T](s:String): Symbol[T] = Symbol(s)

  implicit def char2Sym[T](c:Char): Symbol[T] = Symbol(c.toString)

  /** ``Expr``'s monadic unit. */
  implicit def expr[T](t:T): Const[T] = Const(t)


  type E[R] = Expr[R]

  /** Lift `Function2` to map from/to ``Expr`` */
  def lift[X1, X2, Y](f: (X1, X2) => Y): (E[X1], E[X2]) => Term[Y] =
    (e1, e2) => Term("ds.expr.lift", e1, e2) { e => f(e(e1), e(e2)) }

  /** Lift ``Function1`` to map from/to ``Expr`` */
  def lift[X, Y](f: X => Y): E[X] => Term[Y] =
    e1 => Term("ds.expr.lift", e1) { e => f(e(e1)) }

  /** Raises ``Real`` to the power of another ``Real``.
    * Matrix multiplication also implements ``Power`` to inherit its symbol ``**`` */



}
