package ds.lab

import ds.lab.MonadicTransformers._

import scala.math.Numeric.BigDecimalIsFractional

object MonadicTransformers {

  trait Expr

  /** Evaluates to a real number / scalar.
    * Engine manages number type / precision (Double, BigDecimal) */
  trait Real extends Expr

  /** Engine knows how to evaluate */
  trait Primitive

  case class Decimal(bd:BigDecimal) extends Terminal(bd) with Real

  val Zero:Real = Decimal(0)
  val One:Real = Decimal(1)

  trait Vec extends Expr

  /** Vec's unit. */
  case class SeqVec(s:Seq[Real]) extends Terminal(s) with Vec

  object Vec {
    /** Unit  */
    def apply(x:Real):Vec = apply(Seq(x))
    def apply(s:Seq[Real]):Vec = SeqVec(s)
  }

  abstract class Terminal[T](val value:T) extends Primitive

  object Terminal {
    def unapply[T](t:Terminal[T]):Some[T] = Some(t.value)
  }

  /** Wraps / names another expression. Example: [[Dot]]*/
  abstract class Composed(val expr:Expr)

  abstract class Evaluates[T] (val eval:Engine => T)

  case class Plus(x:Real, y:Real) extends Real with Primitive

  case class Times(x:Real, y:Real) extends Real with Primitive

  case class Map(v:Vec, f:Real=>Real) extends Composed (
    FlatMap(v, f andThen Vec.apply) ) with Vec

  case class FlatMap(v:Vec, f:Real=>Vec) extends
    Evaluates (e => e(f(e(v))) ) with Vec

  case class Join(v:Vec, w:Vec, op:(Real, Real)=>Real) extends Evaluates (
     e => e[Seq[Real]](v).zip(e(w)).map( x => op(x._1,x._2))
    ) with Vec

  case class ForEach(v:Vec,f:Real=>Unit) extends Evaluates (
    _ => Map(v, f andThen {_ => Zero })
  ) with Vec

  case class Fold(v:Vec, start:Real, op:(Real, Real)=>Real) extends Evaluates (
    e => {
      var collector = start
      Map(v, (next:Real) => {
          collector = op(collector,next)
          collector
        })


    }
    ) with Real

  case class Dot(v:Vec, w:Vec) extends Composed (
    Fold(Join(v,w,Times), Zero, Plus) ) with Real

  trait Engine {
    def apply[T](expr:Expr): T
  }

  case class BigDecimalEngine() { self =>

    private val fractional = new BigDecimalIsFractional() {
      def compare(x:BigDecimal, y:BigDecimal):Int = (x - y).toInt
    }

    def apply(v:Vec): Seq[Real] = ???

    def apply[T](expr:Expr): T = ???
    /* @todo cannot convert results to T
    expr match {
      case _:Primitive => {
        case Plus(x,y) => fractional.plus(self[BigDecimal](x),self(y))
        case Terminal(value) => value
        case _ => throw new UnsupportedOperationException(self + "does not support Primitive " + expr)
      }
      case c:Composed => this(c.expr)
      case e:Evaluates[_] => e.eval(self)
      case _ => throw new UnsupportedOperationException(self + "does not support " + expr)
    }
      */



  }


}
