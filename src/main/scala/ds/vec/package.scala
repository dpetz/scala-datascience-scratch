package ds

import ds.expr.Infix._
import ds.expr._
import ds.num._
import ds.vec.Vec
import parser.{Json, Parser}

package object vec {

  type E[R] = Expr[R]
  type S[R] = Seq[R]


  def sum[R](v:E[S[R]])(implicit real:Real[R]):E[R] =
    Term("sum",v) { e=> e(v).foldLeft(real.zero)(real.plus)}

  def dot[R:Real](v:E[S[R]],w:E[S[R]]): E[R] = Named("dot") { sum( v * w ) }

  def norm[R](p:Expr[R], v:E[S[R]])(implicit r:Real[R]):Expr[R] =
    Named("norm") { p match {
      case r.one => v.each(ds.num.abs).all(sum)
      case p => v.each((x:E[R]) => x ** p).all(sum) ** (1 / p)
    }}

  implicit def timesVec[R:Real]: Times[S[R]] = (v,w) => (v zip w)(_ * _)

  implicit def divVec[R:Real]: Div[S[R]] = (v,w) => (v zip w)(_ / _)

  implicit def plusVec[R:Real]: Plus[S[R]] = (v,w) => (v zip w)(_ + _)

  implicit def minusVec[R:Real]: Minus[S[R]] = (v,w) => (v zip w)(_ - _)

  implicit def seq2Vec[T](s:Seq[T]): Vec[T] = Vec(expr(s))

  implicit def vec2Expr[T](v:Vec[T]): Expr[Seq[T]] = v.expr_v

  implicit def lift[T](s:Seq[Expr[T]]): Expr[Seq[T]] = Term("ds.vec.lift",s) { e => s.map(e(_)) }

  implicit def convertElements[X,Y](s: Seq[X])(implicit f: X => Y):Seq[Y] = s.map(f)


  implicit class Vec[X](val expr_v:E[S[X]]) {

    /** map */
    def each[Y](f:E[Y]=>E[Y]):E[S[Y]] =
      Term(expr_v,f) { e => e(expr_v) map (x => e(f(x))) }
    /** reduce */
    def all[Y](f:E[Seq[X]]=>E[Y]):E[Y] = f(expr_v)

    /** Zip and apply binary operation */
    def zip[Y,Z](expr_w:E[S[Y]])(f:(E[X],E[Y])=>E[Z]): Term[S[Z]] =
      Term("ds.vec.zip",expr_v,expr_w)(e => ( e(expr_v) zip e(expr_w) ) map (pair => e(f(pair._1,pair._2))))

    /**
      * @see https://www.scala-lang.org/api/2.12.4/scala/collection/SeqView.html
      */
    def update(i:Int, f:E[X]=>E[X]):E[S[X]] =
      Term("ds.vec.update",this,f){ e =>
          e(expr_v).view.updated(i,e(f(e(expr_v)(i))))
      }

  }


  /** Parse from json string */
  def vec[R:Real](json:String): Term[S[R]] = Term("ds.vec.Vec",json) { _ =>
    val parser: Parser[Json.Arr] = Json.Parsers.arrOf(Json.Parsers.num)
    val r = implicitly[Real[R]]

    Json(json, parser).toArr.values.map { j: Json => r.json(j.toNum) }
  }


}
