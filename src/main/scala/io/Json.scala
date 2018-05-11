package io

import io.parse._
import io.parse.Parser.Spaces
//{Cons, Or, Parser, Rep, Rex, Then, Trans, Gate, Pre, Post, Spaces}

// https://docs.scala-lang.org/style/scaladoc.html#general-style

/**
  * Namespace for Json parser.
  */
object Json {

  /** Parses number values */
  val pNum = Rex(25)("""[\d-+.eE]+""") > { Num(_) }

  /** Parses string values.
    * @todo Escapes, see http://www.json.org */
  val pStr =  Gate("\"",Rep(Rex(1)("[^\"]")),"\"") > { Str(_) }

  /** Parses 'null' string as [[Null]] value */
  val pNull = Cons("null") > { _ => Null }

  val pTrue = Cons("true") > { _ => True }

  val pFalse = Cons("false") > { _ => False }


  def pSpaces[A](p:Parser[A]):Parser[A]=Gate(Spaces,p,Spaces)

  /** @todo pArr without cyclic reference? */
  val pJson  =  pSpaces(pNum | pStr | pNull | pTrue | pFalse)

  val pArr = Gate("[",Then(pJson,Rep(Pre(",",pJson))),"]") > { Arr(_) }

  val pPair = pStr ~ ":" ~ pJson

  val pObj = Gate("{",Then(pPair,Rep(Pre(",",pPair))),"}")

  /** JSON object */
  sealed trait Json

  /** Array */
  case class Arr (values:Seq[Json]) extends Json {
    override def toString = values.mkString("[",",","]")
  }
  /** Object */
  case class Obj (pairs:Map[String,Json]) extends Json
  /** Symbol */
  sealed class Sym(override val toString:String) extends Json
  /** Value */
  sealed trait Val[A] extends Json { def value:A}

  case object True extends Sym("true")
  case object Null extends Sym("null")
  case object False extends Sym("false")

  case class Num(value:Double) extends Val[Double] {
    override def toString = value.toString
  }
  object Num {  def apply(s:String):Num=Num(s.toDouble) }

  case class Str(value:String) extends Val[String] {
    override def toString = s""""$value""""
  }
  object Str { def apply(cs:Seq[String])=new Str(cs.mkString("")) }

}