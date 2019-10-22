package ds.calc

import ds.expr.{Engine, Expr}
import ds.num.Real
import ds.vec.Implicits._
import ds.expr.Implicits._
import ds.num.Implicits._
import ds.vec.Vec
import ds.expr.Functions._

import scala.annotation.tailrec


/**
  * Gradient descent algorithms. Each instance represents a single immutable step.
  * Algorithm can be advanced step-wise via [[next]] or until convergence via [[minimize]] and [[maximize]].
  * [[history]] collects all steps so far as sequence .
  *
  * @param gradient gradient including function
  * @param x Current x/coordinates
  * @param previous Previous descent step (if any)
  */
 class Descent [R:Real] (val gradient:Gradient[R], val x:Vec[R], val previous:Option[Descent[R]]=None) {

 val real = implicitly[Real[R]]

 val engine:Engine = new Engine() // @todo implement w/o engine

  /** Current value at */
  def value:R=gradient.f(x)

  /** New [[Descent]] object at location x. Overwrite to create instance of subclass. */
  def next(x:Vec[R])= new Descent(gradient,x,Some(this))

  /** Explore steps and return position with lowest value. */
  def explore:Vec[R] =
    steps.each { step:Expr[R] => x + (gradient(x) * step) }.map( _ minBy gradient.f)

  /** Possible step widths in this iteration.
    * Overwrite for different (incl dynamic) values */
  val steps:Vec[R]= List(100,10,1,.1,.01,.001,.0001,.00001).map(real.apply)

  /** Accept next candidate location?
    * Overwrite for different (incl. dynamic) tolerance levels. */
  def stop(nextV:Vec[R]):Expr[Boolean] = value ~ gradient.f(nextV)

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:Descent[R]={
    val smallest = explore
    if ( engine(stop(smallest)) ) return this
    next(smallest).minimize
  }

  def maximize:Descent[R]={
   throw UnsupportedOperationException
    //new Descent(-gradient,x,None).minimize
  }

  /* Iterate beginning first until this step */
  def history:List[Descent[R]]= this :: ( previous map (_.history) getOrElse Nil )
}

object Descent {
 def apply [R:Real] (gradient:Gradient[R], x:Vec[R]) = new Descent[R](gradient,x)

}