package ds.expr

import ds.num.Real
import ds.expr.Engine.Config
import ds.func.F1
import ds.matrix.Layout.Rows
import ds.matrix.{All, Matrix, Orientation}


/** Engine for `Real` arithmetic*/
class Engine(private val configs:List[Config]=Engine.defaults,
             private val vars:Map[Symbol[_],_] = Map.empty) {

  //val real: Real[R] = implicitly[Real[R]]

  /** Gets (last added) configuration by type */
  def config[C <: Config](s:String):C = configs.find(_.name == s).asInstanceOf[C]

  /** Reconfigure engine. */
  def update(c:Config):Engine = new Engine(c :: configs, vars)

  def apply[A](e:Expr[A]): A = e match {
    case v:Symbol[A] => vars(v).asInstanceOf[A]
    case c:Closed[A] => c.eval(this)
    case _ => throw Engine.Exception(this,e)
  }

  def apply[I,O](f:F1[I,O], x:I): O = (new Engine(configs, vars(f.x)=x))[O](f)

  import reflect.runtime.universe._

  // https://www.scala-lang.org/api/current/scala-reflect/scala/reflect/api/TypeTags.html

  def real[R]()(implicit tag: TypeTag[R]):Real[R] = tag.tpe match {
    case Double => ds.num.DoubleReal.Real[Double]
    case BigDecimal => ds.num.BigReal.Real[Double]
    case _ => throw Engine.Exception(this, "No algebra for " + tag)
  }
}

object Engine {

  def defaults: List[Config] = List (All(true), All(false), Rows())

  trait Config {
    val name:String
  }

  case class Exception[R](eng:Engine, expr:Expr[R]) extends RuntimeException {
    override def toString = s"Engine $eng cannot evaluate $expr"
  }

}


