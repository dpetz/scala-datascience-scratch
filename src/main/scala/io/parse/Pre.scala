package io.parse
import io.Reader

  case class Pre[A,B](pre:Parser[A], p:Parser[B]) extends Parser[B] {
    def apply(r:Reader)= pre(r) { m => p(m.follow)  }

  }