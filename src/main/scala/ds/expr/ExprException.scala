package ds.expr

case class ExprException(msg:String, e:Expr[_])
  extends RuntimeException(msg + " in: " + e)
