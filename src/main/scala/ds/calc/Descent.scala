package ds.calc

import ds.lina._
import scala.annotation.tailrec
import ds.lina.Vec.{VecMath, Vec}
import ds.num.Real._
import ds.num.Real


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

  /** Current value at */
  def value:R=gradient.f(x)

  /** New [[Descent]] object at location x. Overwrite to create instance of subclass. */
  def next(x:Vec[R])= new Descent(gradient,x,Some(this))

  /** Explore steps and return position with lowest value. */
  def explore():Vec[R] =
    steps map { (x - gradient(x)) * _ } minBy { gradient.f } //recode (NaN, PositiveInfinity) }


  /** Possible step widths in this iteration.
    * Overwrite for different (incl dynamic) values */
  val steps:Vec[R]=Vec(List(100,10,1,.1,.01,.001,.0001,.00001))

  /** Accept next candidate location?
    * Overwrite for different (incl. dynamic) tolerance levels. */
  def stop(nextV:Vec[R]):Boolean= value ~ gradient.f(nextV)

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:Descent[R]={
    val smallest = explore()
    if ( stop(smallest) ) return this
    next(smallest).minimize
  }

  def maximize:Descent[R]={
    new Descent(-gradient,x,None).minimize
  }

  /* Iterate beginning first until this step */
  def history:List[Descent[R]]= this :: ( previous map (_.history) getOrElse Nil )
}

object Descent {
 def apply [R:Real] (gradient:Gradient[R], x:Vec[R]) = new Descent[R](gradient,x)
}