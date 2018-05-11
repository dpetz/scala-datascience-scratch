package io.parse
import io.Reader
import scala.collection.mutable.ArrayBuffer

  /** Parse next n characters from stream into string */
  case class Chars(n: Int) extends Parser[String] {
    
    def apply(start: Reader) = {
      val buf = new ArrayBuffer[Char](n)

      def consume(r: Reader, i: Int) = {
        buf += r.char;
        r.next
      }

      val end = (1 to n).foldLeft(start)(consume)
      Match(buf.mkString(""), end)
    }

  }