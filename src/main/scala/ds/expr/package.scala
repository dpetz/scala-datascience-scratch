package ds


package object expr {

  implicit def str2Sym(s:String) = Symbol(s)

  implicit def char2Sym(c:Char) = Symbol(c)

}
