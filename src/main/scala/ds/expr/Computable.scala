package ds.expr

class Computable[R](f:Engine[R] => R) extends Expr[R]