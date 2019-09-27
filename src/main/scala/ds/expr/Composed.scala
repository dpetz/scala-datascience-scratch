package ds.expr

case class Composed[R,A](expr:Engine[R]=>E[A]) extends Expr[R,A]