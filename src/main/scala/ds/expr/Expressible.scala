package ds.expr

/** Reifies another expression */
trait Expressible[T] extends Expr[T] {
  def express:Expr[T]
  def eval:Engine=>T = express.eval

}
