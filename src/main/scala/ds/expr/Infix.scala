package ds.expr

object Infix {

  type Binary[T] = (Expr[T],Expr[T])=>Expr[T]
  type Relation[T] = (Expr[T],Expr[T])=>Expr[Boolean]

  trait TimesTimes[T] extends Binary[T]
  trait Plus[T] extends Binary[T]
  trait Minus[T] extends Binary[T]
  trait Div[T] extends Binary[T]
  trait Times[T] extends Binary[T]

  trait Negate[T] extends (Expr[T]=>Expr[T])

  trait Approx[T] extends Relation[T]

  trait Compare[T] extends ((Expr[T],Expr[T])=>Expr[Int])


}
