package io.parse
import io.Reader

/** Repeats parsing at least `min` an at most `max`. */
  case class Optional[A](p: Parser[A]) extends Parser[Seq[A]] {
	def apply(r: Reader) = Repeat(p,0,1)(r)
  }
