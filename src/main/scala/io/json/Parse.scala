package io.json

import io.parse._

object Parse {

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
  def spaces[A](p:Parser[A]):Parser[A]=Gate(Parser.Spaces,p,Parser.Spaces)

  val json:Parser[Json] = spaces(num | str | nul | tru | fal | arr | obj)
  
  val arr:Parser[Arr] =
    Gate("[",Then(json,Repeat(Prefix(",",json))),"]") > { Arr(_) }

 val pair:Parser[(String,Json)] = Then(Postfix(str, ":"), json) > {
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

