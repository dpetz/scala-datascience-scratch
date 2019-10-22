package ds

import ds.expr.Infix.Times
import ds.expr._
import ds.num._
import parser.{Json, Parser}

package object vec {

  type E[R] = Expr[R]
  type S[R] = Seq[R]


  def sum[R](v:E[S[R]])(implicit real:Real[R]):E[R] =
    Term("sum",v) { e=> e(v).foldLeft(real.zero)(real.plus)}

  def dot[R:Real](v:E[S[R]],w:E[S[R]]): E[R] = sum( v * w )

  def norm[R](p:Expr[R], v:E[S[R]])(implicit r:Real[R]):Expr[R] =  p match {
    case r.one => v.each(ds.num.abs).all(sum)
    case p => v.each((x:E[R]) => x ** p).all(sum) ** (1 / p)
  }

  implicit def timesVec[R:Real]: Times[S[R]] = (v,w) => (v op w)(_ * _)

  implicit def divVec[R:Real]: Times[S[R]] = (v,w) => (v op w)(_ / _)

  implicit def plusVec[R:Real]: Times[S[R]] = (v,w) => (v op w)(_ + _)

  implicit def minusVec[R:Real]: Times[S[R]] = (v,w) => (v op w)(_ - _)

  implicit def seq2Expr[T](s:Seq[T]): SeqExpr[T] = SeqExpr(expr(s))

  implicit class SeqExpr[X](val v:E[S[X]]) {
    /** map */
    def each[Y](f:E[Y]=>E[Y]):E[S[Y]] =
      Term(v,f) { e => e(v) map (x => e(f(x))) }
    /** reduce */
    def all[Y](f:E[Seq[X]]=>E[Y]):E[Y] = f(v)

    /** Zip and apply binary operation */
    def op[Y,Z](w:E[S[Y]])(f:(E[X],E[Y])=>E[Z]): Term[S[Z]] =
      Term("ds.vec.zip",v,w)(e => ( e(v) zip e(w) ) map ( pair => e(f(pair._1,pair._2))))

    /**
      * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      */

    def update(i:Int, f:E[X]=>E[X]):E[S[X]] =
      Term("ds.vec.update",this,f){ e =>
          e(v).view.updated(i,e(f(e(v)(i))))
      }

  }


  /** Parse from json string */
  def vec[R:Real](json:String): Term[S[R]] = Term("ds.vec.Vec",json) { _ =>
    val parser: Parser[Json.Arr] = Json.Parsers.arrOf(Json.Parsers.num)
    val r = implicitly[Real[R]]

    Json(json, parser).toArr.values.map { j: Json => r.json(j.toNum) }
  }


}
