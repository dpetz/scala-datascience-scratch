package ds


package object expr {

  type E[T] = Expr[T]

  implicit def str2Sym[T](s:String): Symbol[T] = Symbol(s)

  implicit def char2Sym[T](c:Char): Symbol[T] = Symbol(c.toString)

  /** ``Expr``'s monadic unit. */
  implicit def expr[T](t:T): Const[T] = Const(t)

  //def id(s:String):String = s"${this.getClass.getName}.$s"

  /** flatMap */
  def next[X,Y](expr:E[X], f:X=>E[Y]): E[Y] = Term("ds.exr.next",expr,f) {
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

  type Binary[X,Y,Z] = ((E[X],E[Y])=>E[Z])

  trait TimesTimes[X,Y,Z] extends Binary[X,Y,Z]
  trait Plus[X,Y, Z] extends Binary[X,Y,Z]
  trait Div[X,Y,Z] extends Binary[X,Y,Z]
  trait Times[X,Y,Z] extends Binary[X,Y,Z]


}
