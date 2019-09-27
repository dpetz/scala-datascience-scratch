package ds.expr

import ds.expr.Engine._
import ds.num.real._

abstract class Engine[R:Real](val layout:Layout=Rows())  {

  def apply[A](e:Expr[R,A]): A

  /* Matrix parameters */
  val rows:Filter = All()

  val cols:Filter = All()

  def transpose:Engine[R] =  layout(layout.transpose)

  def layout(l:Layout):Engine[R]

  def turn:Engine[R] = layout(layout.transpose)


}



object Engine {


  trait Filter {
    def apply(i:Int):Boolean
  }

  case class All() extends Filter {
    def apply(i:Int):Boolean = true
  }

  sealed abstract class Layout {
    def transpose:Layout
  }

  case class Rows() extends Layout {
    def transpose = Columns()
  }
  case class Columns() extends Layout {
    def transpose = Rows()
  }




}