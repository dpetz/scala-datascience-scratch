package ds.expr

case class Composed[R](expr:()=>E[R]) extends Expr[R]