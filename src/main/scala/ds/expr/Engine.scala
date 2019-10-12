package ds.expr

import ds.num.Real
import ds.expr.Engine.Config
import ds.func.Func.F1


/** Engine for `Real` arithmetic*/
class Engine[R:Real](private val configs:List[Config]=Engine.defaults,
             private val vars:Map[Symbol[_],_] = Map.empty) {

  val real: Real[R] = implicitly[Real[R]]

  /** Gets (last added) configuration by type */
  def config[C <: Config](s:String):C = configs.find(_.name == s).asInstanceOf[C]

  /** Reconfigure engine. */
  def update(c:Config):Engine = new Engine(c :: configs, vars)

  def apply[A](e:Expr[A]): A = e match {
    case v:Symbol[A] => vars(v).asInstanceOf[A]
    case _ => e(this)
  }

  def apply[I,O](f:F1[I,O], x:I): O = (new Engine(configs, vars(f.x)=x))[O](f)

  import reflect.runtime.universe._

  // https://www.scala-lang.org/api/current/scala-reflect/scala/reflect/api/TypeTags.html
  /*
  def real[R]()(implicit tag: TypeTag[R]):Real[R] = tag.tpe match {
    case Double => ds.num.DoubleReal.Real[Double]
    case BigDecimal => ds.num.BigReal.Real[Double]
    case _ => throw EngineException(this, "No algebra for " + tag)
  }
   */
}



object Engine {

  def defaults: List[Config] = List (All(true), All(false), Rows())

  trait Config {
    val name:String
  }

  val MATRIX_LAYOUT = "Matrix Layout"
  val ROW_FILTER = "Row Filter"
  val COLUMN_FILTER = "Column Layout"



  abstract class Filter(rows:Boolean = true) extends Config {
    val name:String = if (rows) ROW_FILTER else COLUMN_FILTER
    def apply(i:Int):Boolean
  }

  case class All(rows:Boolean) extends Filter(rows) {
    def apply(i:Int):Boolean = true
  }

  sealed abstract class Layout() extends Config {
    val name:String = MATRIX_LAYOUT
    def transpose:Layout
  }

  case class Rows() extends Layout {
    def transpose = Columns()
  }
  case class Columns() extends Layout {
    def transpose = Rows()
  }




}

case class EngineException[R](eng:Engine, expr:Expr[R]) extends Exception {
  override def toString = s"Engine $this cannot evaluate $expr"
}
