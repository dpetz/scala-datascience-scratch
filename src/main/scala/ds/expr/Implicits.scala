package ds.expr

object Implicits {

  type Scalar[R] = Expr[R]


  implicit def str2Sym[T](s:String): Symbol[T] = Symbol(s)

  implicit def char2Sym[T](c:Char): Symbol[T] = Symbol(c.toString)

  /** ``Expr``'s monadic unit. */
  implicit def expr[T](t:T): Const[T] = Const(t)


}