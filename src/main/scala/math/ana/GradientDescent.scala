package math.ana

import scala.util.Random
import math.lina._
import scala.annotation.tailrec

import GradientDescent._

/**
  * Gradient descent algorithms as a [[scala.collection.Traversable]] of its steps
  * (most recent step last)
  *
  * @param gradient gradient including function
  * @param position Current position/coordinates
  * @param steps Sequence of positionsible step sizes. Defaults to [[GradientDescent.Grid]].
  * @param stop Predicate if continue
  * @param previous Previous descent step (if any)
  */
case class GradientDescent(gradient:Gradient, position:Vec, steps:Steps = new Grid, stop:Stop = new Stop,
                            previous:Option[GradientDescent]=None) extends Traversable[GradientDescent] {

  /** Current value at */
  def value:Double=gradient.func(position)

  /**
    * Try one step.
    * @param v current position
    * @param direction direction of descent
    * @param stepSize candidate stepsize
    * @return new position
    */
  private def step(v:Vec, direction:Vec, stepSize:Double):Vec =
    v.zip(direction).map { case (vi,zi) => vi + stepSize* zi }


  private def recodeNaN(x:Double):Double =
    if (x == Double.NaN) Double.PositiveInfinity else x

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:GradientDescent={
    val neggradient = gradient(position).map(-_)
    val vMinFunc = steps(this) map { step(position,neggradient,_) } minBy { (gradient.func) wrap recodeNaN }
    if ( stop(this,vMinFunc) ) return this
    new GradientDescent(gradient,vMinFunc,steps,stop,Some(this)).minimize
  }

  def maximize:GradientDescent={
    GradientDescent(gradient.negate,position,steps,stop,None).minimize
  }

  /* Iterate beginning first until this step */
  override def foreach[U](f: (GradientDescent) => U)= {
    previous.foreach(_.foreach(f))
    f(this)
  }
}

object GradientDescent {

  /** For a requesting gradient descent iteration returns eligable step sizes. */
  trait Steps extends (GradientDescent => Seq[Double])

  /** Standard implementation of [[Steps]] returning same grid for all steps. */
  case class Grid(grid:Seq[Double]=List(100,10,1,.1,.01,.001,.0001,.00001)) extends Steps {
    def apply(gd:GradientDescent) = grid
  }

  /** For a calling gradient descent decides if to stop because objective
    * does not improve beyond tolerance. */
  class Stop(val tolerance:Double=0.000001) extends ((GradientDescent, Vec) => Boolean) {
    def apply(gd:GradientDescent, nextV:Vec)=
      Math.abs(gd.value - gd.gradient.func(nextV)) < tolerance
  }
}


