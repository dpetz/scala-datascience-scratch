package ds.ana

import ds.lina
import Function.tupled

/** Gradient of real-valued function
  * Estimated via difference quotient unless provided analytically
  * @param func Function mapping vectors to doubles
  * @param derivatives (Optional) analytical derivatives
  * @param h tiny constant to approximate limit for difference quotient
  *
  * */
class Gradient(val func:Vec=>Double, val derivatives:Option[(Vec,Int)=>Double]=None, val h:Double = 0.00001) {


  /** Compute the ith partial difference quotient of f at v */
  private def estimate(v:Vec, i:Int):Double={
    val w = v.zipWithIndex.map {
      case (x, j) => if (j==i) x+h else x
    }
    return (func(w) - func(v)) / h
  }

  /** Estimates gradient. Computational expensive (2n function evaluations)
    * and estimation error can be substantial, see Gradient.test
    * @return vector of partial difference quotients
    * */
  //def estimate(v:Vec):Vec= v.indices.map { estimate(v,_) }

  /** Returns (derivatives or estimated) gradient at v */
  def apply(v:Vec):Vec = (v indices) map (apply(v,_))

  /** Returns (derivatives or estimated) partial derivatives at v and index i */
  def apply(v:Vec,i:Int):Double =
    derivatives map (_(v,i)) getOrElse estimate(v,i)

  /* Negates function and derivatives gradient (if any) */
  def negate:Gradient=new Gradient( func.negate,derivatives map (_.negate),h )
}

object Gradient {

  def apply(f:Vec=>Double,g:(Vec,Int)=>Double)=new Gradient(f,Some(g))
  def apply(f:Vec=>Double)=new Gradient(f)
}