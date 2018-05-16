/**
  * Json parsers and representations.
  * @todo Complete doc
  * @see [[http://www.json.org]]
  * @see [[https://docs.scala-lang.org/style/scaladoc.html#general-style]]
  */
package io

import io.parse._
import io.parse.Parser.Spaces
import scala.util.{Try, Success, Failure}
//import scala.collection.AbstractTraversable

/** JSON object */
sealed trait Json { //extends Traversable[Json] {
  //def values:Traversable[Json]
  //def foreach(f:(Json) => Unit ):Unit = values.foreach(f)
}


sealed trait Collection extends Json {
	def values:Seq[Json]
}

/** JSON Object */
case class Obj (pairs:Map[String,Json]) extends Json {
  def values = pairs.values
  override def toString = pairs.iterator.map{
  	case (k,v) => s""""$k":$v"""}.mkString("{",",","}")
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
object Str {
  /** Constructs from sequence of [[String]]s */
  def apply(cs:Seq[String]) = new Str(cs.mkString(""))
}

/** JSON symbol: [[True]], [[False]], or [[NUll]]. */
sealed class Sym(override val toString:String) extends Json

/** JSON `true` singleton. */
case object True extends Sym("true")

/** JSON `null` singleton. */
case object Null extends Sym("null")

/** JSON `false` singleton. */
case object False extends Sym("false")

object Json {

  /** Parses [[Json]] from string or exception if not a valid Json. */
  def parse(parseMe:String):Try[Json] =
  	Parsers.JsonParser()(parseMe) match {
  		case m:Match[Json] => Success(m.result)
  		case f:Fail[Json]  => Failure(ParseException(f))
  	}

  /** Shortcut for [[parse(String).get]] for strings known to be well-formed.
   * @throws [[io.parse.Parser.ParseException]] if parsing fails */
  def apply(parseMe:String):Json = parse(parseMe).get

   object Parsers {

	  /** Parses number values */
	  val num = Regex(25)("""[\d-+.eE]+""") > { Num(_) }

	  /** Parses string values.
	    * @todo Escapes, see http://www.json.org */
	  val str =  Gate("\"",Repeat(Regex(1)("[^\"]")),"\"") > { Str(_) }

	  /** Parses "null" into [[Null]]  */
	  val nul = Cons("null") > { _ => Null }

	  /** Parses "true" into [[True]]  */
	  val tru = Cons("true") > { _ => True }

	  /** Parses "false" into [[False]]  */
	  val fal = Cons("false") > { _ => False }

	  /** Trims whitespaces if any before parsing
	   * @see [[Spaces]]
	   */
	  def spaces[A](p:Parser[A]):Parser[A]=Gate(Spaces,p,Spaces)
  
	  val arr = Gate("[",Then(JsonParser(),Repeat(Prefix(",",JsonParser()))),"]") > { Arr(_) }

	  val emptyArr = Then(Cons("["),spaces("]")) > { _ => Arr(Nil)}

	  case class JsonParser() extends Parser[Json] {
	    def apply(r: Reader):Result[Json] = spaces ( num | str | nul | tru | fal | arr | obj | emptyArr) (r) 
	  }

	 val pair:Parser[(String,Json)] = Then(Postfix(spaces(str), ":"), JsonParser()) > {
	    seq:Seq[Json] => (
	      seq(0).asInstanceOf[Str].value,
	      seq(1)
	    )
	  }

	  val obj:Parser[Obj] =
	    Gate("{",Then(pair,Repeat(Prefix(",",pair))),"}")  > {
	      m:Seq[(String,Json)] => Obj(m.toMap)
	  }

 }
}

