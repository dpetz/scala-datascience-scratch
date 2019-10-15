package ds.expr


trait Monad[Expr[_]]  {

  def unit[R](x:()=>R):Expr[R]

  def flatMap[R,S](e:Expr[R])(f:R=>Expr[S]):Expr[S]

}
