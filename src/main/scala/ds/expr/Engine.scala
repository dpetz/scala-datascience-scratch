package ds.expr

import ds.expr.Engine._
import ds.func.{Assign, Func, Symbol}
import ds.num.Real

import scala.collection.immutable.ListMap
import scala.collection.mutable

abstract class Engine(val layout:Layout=Rows())  {

  def apply[T](e:Expr[T]): T

  /** Gets (last added) configuration by type */
  def config[C <: Config](s:String):C

  /** Reconfigure engine. */
  def update(c:Config):Engine

  def apply[T](e:Func[T], x:T): T


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