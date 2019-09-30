package ds.expr

import ds.num.Real

/** Expression in two arguments evaluating to a ``Boolean`` */
 abstract class Relation[R:Real](val x:Expr[R], val y:Expr[R]) extends Expr[Boolean]