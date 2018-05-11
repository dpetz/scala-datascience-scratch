package io.parse
import io.Reader

/** Translate */
  case class Trans[A,B](p:Parser[A],f:A=>B) extends Parser[B] {
    def apply(r:Reader)=p(r) map { f(_) }
  }