package ds.expr

case class Composed[R](expr:Engine[R]=>E[R]) extends Expr[R]