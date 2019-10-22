package ds.expr


 object Functions {

   type E[T] = Expr[T]

  //def id(s:String):String = s"${this.getClass.getName}.$s"

  /** flatMap */
  def transform[X,Y](expr:E[X])(f:X=>E[Y]): E[Y] = Term("ds.expr.next",expr,f) {
    e => e(f(e(expr)))
  }

  def map[S,T](x:E[S],f:E[S]=>E[T]):Term[T] = Term ("ds.expr.map",x,f) { e => e(f(x)) }

  /** Lift `Function2` to transform from/to ``Expr`` */
  def lift[X1, X2, Y](f: (X1, X2) => Y): (E[X1], E[X2]) => Term[Y] =
    (e1, e2) => Term("ds.expr.lift", e1, e2) { e => f(e(e1), e(e2)) }

  /** Lift ``Function1`` to transform from/to ``Expr`` */
  def lift[X, Y](f: X => Y): E[X] => Term[Y] =
    e1 => Term("ds.expr.lift", e1) { e => f(e(e1)) }

  /** Raises ``Real`` to the power of another ``Real``.
    * Matrix multiplication also implements ``Power`` to inherit its symbol ``**`` */




}
