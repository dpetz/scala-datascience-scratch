package ds.expr

/** Variable */
case class Symbol[T](name:String) extends Expr[T] {
  def parts:Seq[Expr[T]] = Nil
}


object Symbol {

  def apply(c:Char) = new Symbol(c.toString)

}



