package ds.expr

object Infix {

  type Binary[X] = ((Expr[X],Expr[X])=>Expr[X])

  trait TimesTimes[X] extends Binary[X]
  trait Plus[X] extends Binary[X]
  trait Minus[X] extends Binary[X]
  trait Div[X] extends Binary[X]
  trait Times[X] extends Binary[X]

  trait Negate[X] extends (Expr[X]=>Expr[X])
  trait Inverse[X] extends (Expr[X]=>Expr[X])
}
