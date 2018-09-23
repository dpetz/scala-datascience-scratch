package ds.calc

import scala.util.Random
import ds.lina._
import ds.algebra._

import scala.annotation.tailrec

import GradientDescent._

/**
  * Gradient descent algorithms as a [[scala.collection.Traversable]] of its steps
  * (most recent step last)
  *
  * @param gradient gradient including function
  * @param x Current x/coordinates
  * @param steps Sequence of possible step sizes. Defaults to [[GradientDescent.Grid]].
  * @param stop Predicate if continue
  * @param previous Previous descent step (if any)
  */
case class GradientDescent(
  func:ScalarField,
  gradient:Gradient,
  x:Vec,
  steps:Steps = Grid(List(100,10,1,.1,.01,.001,.0001,.00001)),
  stop:Stop = Tolerance(0.000001),
  previous:Option[GradientDescent]=None
  ) {

  /** Current value at */
  def value:Double=func(x)

  /**
    * Try one step.
    * @param x current position
    * @param direction direction of descent
    * @param stepSize candidate stepsize
    * @return new position
    */
  private def explore(x:Vec, direction:Vec, stepSize:Double):Vec =
    x + direction * stepSize

  def step():Vec =
    steps(this) map { explore(x,-gradient(x),_) } minBy { func recode (NaN, PositiveInfinity) }

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:GradientDescent={
    val next = step()
    if ( stop(this,next) ) return this
    new GradientDescent(func,gradient,next,steps,stop,Some(this)).minimize
  }

  def maximize:GradientDescent={
    GradientDescent(func.negate,gradient.negate,x,steps,stop,None).minimize
  }

  /* Iterate beginning first until this step */
  def history:List[GradientDescent]= this :: ( previous map (_.history) getOrElse Nil )
}

object GradientDescent {

  type Steps = (GradientDescent => Vec) 
  type Stop = ((GradientDescent, Vec) => Boolean)

  /** For a requesting gradient descent iteration returns eligable step sizes.
  * Standard implementation of [[Steps]] returning same grid for all steps. */
  case class Grid(values:Vec) extends Steps {
    def apply(g:GradientDescent)=values
  }

  /** For a calling gradient descent decides if to stop because objective
    * does not improve beyond tolerance. */
  case class Tolerance(h:Double) extends Stop {
    def apply(gd:GradientDescent, nextV:Vec)=
      Math.abs(gd.value - gd.func(nextV)) < h
  }
}


