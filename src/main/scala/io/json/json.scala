/**
  * Json parsers and representations.
  * @todo Complete doc
  * @see [[http://www.json.org]]
  * @see [[https://docs.scala-lang.org/style/scaladoc.html#general-style]]
  */
package io.json

import io.parse._
//import scala.collection.AbstractTraversable

/** JSON object */
sealed trait Json { //extends Traversable[Json] {
  //def values:Traversable[Json]
  //def foreach(f:(Json) => Unit ):Unit = values.foreach(f)

}

/** JSON Object */
case class Obj (pairs:Map[String,Json]) extends Json {
  //def values = pairs.values
  override def toString = pairs.iterator.map{ case (k,v) => s"$k:$v"}.mkString("{",",","}")
}

/** JSON Array */
case class Arr (values:Seq[Json]) extends Json {
  override def toString = values.mkString("[",",","]")
}

/**JSON literal, i.e. [[Str]] or [[Num]]. */
sealed trait Lit[A] extends Json {
	def value:A
	//def values = List(value)
}

/** JSON number */
case class Num(value:Double) extends Lit[Double] {
  override def toString = value.toString
}
/** Provides constructor from [[String]] */
object Num {
	def apply(s:String):Num=Num(s.toDouble)
}

/** JSON string */
case class Str(value:String) extends Lit[String] {
  override def toString = s""""$value""""
}
/** Provides constructor from sequence of [[String]]s */
object Str {
  def apply(cs:Seq[String]) = new Str(cs.mkString(""))
}

/** JSON symbol, i.e. [[True]], [[False]], or [[NUll]]. */
sealed class Sym(override val toString:String) extends Json
/** JSON `true` singleton. */
case object True extends Sym("true")
/** JSON `null` singleton. */
case object Null extends Sym("null")
/** JSON `false` singleton. */
case object False extends Sym("false")