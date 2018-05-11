package io.parse
import io.Reader

  case class Or[A](p1:Parser[A], p2:Parser[A]) extends Parser[A] {
    def apply(r: Reader) = p1(r).atFail { _ => p2(r) }
  }
