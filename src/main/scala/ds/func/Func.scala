package ds.func

import ds.expr.{Engine, Expr}

/** ``Expr``that require inputs to evaluate */
trait Func[Y] extends Expr[Y] {
  def eval[R](e:Engine[R]) = throw UnsupportedOperationException
  def name:String
  /** Returns ``Nil`` unless overwritten. */
  def parts:Seq[Expr[_]] = Nil
}