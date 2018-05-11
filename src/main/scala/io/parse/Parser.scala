package io.parse

//import io.Parser._
//{Cons, Fail, Result, Rep, Rex}

import io.Reader

  /* Can be applied to a Stream to produce a Match[A] */
  trait Parser[+A] {
    /* Parse from character stream. */
    def apply(r: Reader): Result[A]

    /* Shortcut for apply(Reader(s)) */
    def apply(s: String): Result[A] = apply(Reader(s))

    /* Infix operator for Then(this,p) */
    def ~[B >: A](p: Parser[B]):Parser[Seq[B]] = Then(this, p)

    def |[B >: A](p: Parser[B]):Parser[B] = Or(this, p)

    def >[B](f:A=>B):Parser[B]=Trans(this,f)

  }


object Parser {

  val Spaces = Rep(OneOf(Set('\n','\t',' ','\r')))

  implicit class Str2Cons(s: String) extends Cons(s)

  implicit class Parser2SeqParser[+A](p: Parser[A]) extends Parser[Seq[A]] {
    def apply(r: Reader) = p(r).map(Vector(_))
  }

  object Test {

    def and {
      //import io.Parser.{Rep, Str2Cons}
      val r = Reader("2017-08-26")
      val p = Rep(Rex(5)("""\d+-?"""))
      println(p(r).toString)
    }
  }
}