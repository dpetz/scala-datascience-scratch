package ds.vec
import ds.expr._
import ds.func.{F1, F2, F3}
import ds.num._


/** Wraps ``Expr`` evaluating to ``Seq[R]`` for vector infix operations
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
abstract class Vec[R](implicit real:Real[R]) extends Expr[Seq[R]] {

  private val vf = Vec.functions(real)
  private val sf = Scalar.functions(real)



  def size:Scalar[R] = vf.size(this)

  def sum: Scalar[R] = vf.sum(this)

  /** Map function expression to every vector element. */
  def map(f:F1[R,R]):Vec[R] = vf.map(f) (this)

  /** Zip two vectors and reduce pairs via function. */
  def zip(f:F2[R,R,R])(w: Vec[R]):Vec[R] = vf.zip(f) (this,w)

  /** Dot product **/
  def dot(w: Vec[R]): Scalar[R] = vf.dot (this, w)

  /** Calculates the p-norm */
  def norm(p: Int): Scalar[R] = vf.norm(p) (this)

  /** Add elementwise */
  def +(w: Vec[R]): Vec[R] = vf.zip (sf.plus) (this, w)

  /** Multiply elementwise */
  def *(w: Vec[R]): Vec[R] = vf.zip (sf.times) (this, w)

  /** Divide elementwise */
  def /(w: Vec[R]): Vec[R] = vf.zip (sf.div) (this, w)

  /** Substract elementwise */
  def -(w: Vec[R]): Vec[R] = vf.zip (sf.minus) (this, w)

  def unary_- : Vec[R] = vf.map (sf.negate) (this)

  /** Add constant */
  def +(x: Scalar[R]): Vec[R] = map (sf.plus ! x)

  /** Multiply constant */
  def *(x: Scalar[R]): Vec[R] = map (sf.times ! x)


}

object Vec {

  def functions[R:Real]:Functions[R] = new Functions[R] // @todo Buffer

  class Functions[R:Real] {

    type VecOps = F2[Seq[R],Seq[R],Seq[R]]
    type RealOps = F2[Seq[R],Seq[R],R]
    type ScalarField = F1[Seq[R],R]

    /** Scalar functions */
    private val sf = Scalar.functions

    /** Zip two vectors and reduce pairs via function. */
    def zip(f:F2[R,R,R]): F2[Seq[R], Seq[R], Seq[R]] = F2(
      "zip", (e,v,w) => ( e(v) zip e(w)) map (x => f.eval(e, x._1, x._2)))

    /** Map function expression to every vector element. */
    def map(f:F1[R,R]):F1[Seq[R],Seq[R]] = F1("map", (e, v) => e(v) map (x => f.eval(e,x)) )

    val times:VecOps = zip (sf.times)

    val sum:ScalarField = F1("sum", (e,v) => e(v).fold(e.real[R].zero)(e.real[R].plus))

    val dot:RealOps = times ° sum

    def norm(p:Int):ScalarField  = p match {
      case 1 => map(sf.abs) ° sum
      case p:_ => map(sf.power !! p) ° sum ° (sf.power !! (1.0 / p))
    }

    val size:ScalarField = F1("size", (e,v) => e(v).size)


  }


}

