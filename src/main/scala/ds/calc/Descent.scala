package ds.calc

import scala.util.Random
import ds.lina._
import ds.algebra._

import scala.annotation.tailrec

import Descent._



/**
  * Gradient descent algorithms. Each instance represents a single immutable step.
  * Algorithm can be advanced step-wise via [[step]] or until convergence via [[minimize]] and [[maximize]].
  * [[history]] collects all steps so far as sequence .
  *
  * @param gradient gradient including function
  * @param x Current x/coordinates
  * @param steps Sequence of possible step sizes. Defaults to [[Grid]].
  * @param stop Predicate if continue. Defaults to [[Neighborhood]]
  * @param previous Previous descent step (if any)
  */
case class Descent(gradient:Gradient,x:Vec,steps:Steps = log10Steps, stop:Stop = new WithinTolerance,
                   previous:Option[Descent]=None) {

  /** Current value at */
  def v:Real=gradient.f(x)

  def step():Vec =
    steps(this) map { x - gradient(x) * _ } minBy { gradient.f } //recode (NaN, PositiveInfinity) }

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:Descent={
    val next = step()
    if ( stop(this,next) ) return this
    Descent(gradient,next,steps,stop,Some(this)).minimize
  }

  def maximize:Descent={
    Descent(gradient.negate,x,steps,stop,None).minimize
  }

  /* Iterate beginning first until this step */
  def history:List[Descent]= this :: ( previous map (_.history) getOrElse Nil )
}

object Descent {

  type Steps = (Descent => Vec)
  type Stop = ((Descent, Vec) => Boolean)

  /** Standard implementation of [[Steps]] returning same grid for all steps. */
  val log10Steps = Grid(List(100,10,1,.1,.01,.001,.0001,.00001))

  /** For a requesting gradient descent iteration returns eligible step sizes. */
  case class Grid(values:Vec) extends Steps {
    def apply(g:Descent)=values
  }

  /** For a calling gradient descent decides if to stop because objective
    * does not improve beyond tolerance. */
  case class WithinTolerance(implicit t:Tolerance[Real]) extends Stop {
    def apply(gd:Descent, nextV:Vec)=
      t.approx(gd.v,gd.gradient.f(nextV))
  }
}


