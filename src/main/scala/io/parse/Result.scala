package io.parse
import io.Reader

  /** Result of applying Parser to Stream */
  sealed trait Result[+A] {

    def follow: Reader

    /** Map result if Match. */
    def map[B](f: A => B): Result[B]

    /** Map this instance if Match. */
    def apply[B](f: (Match[A] => Result[B])): Result[B]

    /** Map this instance if if Fail **/
    def atFail[B >: A](f: Fail[A] => Result[B]): Result[B]

    //def flat[B]:Parsed[B]

    //def asMatch:Match[A]
  }


  /** @param follow : Reader after parsed content has been consumed */
  case class Match[+A](result: A, follow: Reader) extends Result[A] {
    def map[B](f: A => B) = Match(f(result), follow)

    def apply[B](f: (Match[A] => Result[B])) = f(this)

    def atFail[B >: A](f: Fail[A] => Result[B]) = this

    /** New match with results combined by function and reader from second match */
    def add[B,C](m:Match[B])(f:(A,B)=>C ):Match[C]=Match(f(result,m.result),m.follow)

    //def asMatch = this
  }

  case class Fail[+A](parser: Parser[_], follow: Reader) extends Result[A] {
    def map[B](f: A => B) = Fail[B](this)

    def apply[B](f: Match[A] => Result[B]) = Fail[B](this)

    override def toString = s"$parser failed parsing: $follow"

    def atFail[B >: A](f: Fail[A] => Result[B]) = f(this)

    //def asMatch = throw new ClassCastException
  }

  object Fail {
    def apply[B](f: Fail[_]): Fail[B] = Fail[B](f.parser, f.follow)
  }

