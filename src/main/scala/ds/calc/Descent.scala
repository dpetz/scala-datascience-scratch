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
  * @param gradient gradient (includes function)
  * @param x Current x/coordinates
  * @param previous Previous descent step (if any)
  */
 case class Descent [R:Real] ( gradient:Gradient[R],  x:Vec[R],  previous:Option[Descent[R]]=None) {

 val real = implicitly[Real[R]]

  /** Current value at ``x`` */
  def value:Expr[R] = gradient.f(x)

  implicit def some[T](obj:T):Option[T] = Some(obj)

  /** New [[Descent]] object at location x. Overwrite to create instance of subclass. */
  def next(x:Vec[R])=  Descent(gradient,x,this)

  /** Possible step sizes in this iteration.
    * Overwrite for different (eg. dynamic) values */
  val steps:Vec[R] = seq2Vec(List(100,10,1,.1,.01,.001,.0001,.00001).map(real.apply))


  /** Explore steps and return position with lowest value.
    * */
  def explore:Vec[R] =
  // First each transforms from Vec[R] to Vec[Seq[R]]]
    steps.each { step:Expr[R] => x + (gradient(x) * step) }.each(
     v:Expr[Seq[R]] =>  gradient.f(seq2Vec(v))

    val candidates: E[E[S[R]]] = for (step <- steps.ex)
      yield (x + (gradient(x) * step)


  )

    )


  /** Accept next candidate location?
    * Overwrite for different (incl. dynamic) tolerance levels. */
  def stop(nextV:Vec[R]):Expr[Boolean] = value ~ nextV.map(gradient.f)

  /** Recursively descent
    * @return last step
    */
  @tailrec final def minimize:Descent[R]={
    val smallest = explore
    // @todo implement w/o engine
    val e = new Engine()
    if ( e(stop(smallest)) ) return this
    next(smallest).minimize
  }

  def maximize:Descent[R]={
   throw new UnsupportedOperationException
    //new Descent(-gradient,x,None).minimize
  }

  /* Iterate beginning first until this step */
  def history:List[Descent[R]]= this :: ( previous map (_.history) getOrElse Nil )
}

object Descent {
 def apply [R:Real] (gradient:Gradient[R], x:Vec[R]) = new Descent[R](gradient,x)

}