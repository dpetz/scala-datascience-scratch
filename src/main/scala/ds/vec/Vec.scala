package ds.vec
import ds.expr._
import ds.func.{F1, F2, F3}
import ds.num._
import parser.{Json, Parser}


/** Wraps ``Expr`` evaluating to ``Seq[R]`` for vector infix operations
  * @see https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet */
abstract class Vec[R](implicit real:Real[R]) extends Expr[Seq[R]] {

  private val vf = Vec.functions(real)
  private val sf = Scalar.functions(real)

  def size:Scalar[R] = vf.size(this)

  def sum: Scalar[R] = vf.sum(this)

  /** Map function expression to every vector element. */
  def map(f:F1[Expr[R],R]):Vec[R] = vf.map(f) (this)

  /** Zip two vectors and reduce pairs via function. */
  def zip(f:F2[Expr[R],Expr[R],R])(w: Vec[R]):Vec[R] = vf.zip(f) (this,w)

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

    type SS = Seq[Seq[R]]

    type VecOps = F2[Vec[R],Vec[R],Seq[R]]
    type RealOps = F2[Vec[R],Vec[R],R]
    type ScalarField = F1[Vec[R],R]
    type E = Expr[R]

    /** Scalar functions */
    private val sf = Scalar.functions

    /** Zip two vectors and reduce pairs via function. */
    def zip(f:F2[E,E,R]): F2[Vec[R], Vec[R], Seq[R]] = F2(
      "zip", (e,v,w) => ( e(v) zip e(w)) map (x => f.eval(e, x._1, x._2)))

    /** Map function expression to every vector element. */
    def map(f:F1[E,R]):F1[Vec[R],Seq[R]] = F1("vec.map", (e, v) => e(v) map (x => f.eval(e,x)) )

    val times:VecOps = zip (sf.times)

    val sum:ScalarField = F1("vec.sum", (e,v) => e(v).fold(e.real[R].zero)(e.real[R].plus))

    val dot:RealOps = times ° sum

    def norm(p:Int):ScalarField  = p match {
      case 1 => map(sf.abs) ° sum
      case p:_ => map(sf.power !! p) ° sum ° (sf.power !! (1.0 / p))
    }


    val size:ScalarField = F1("vec.size", (e,v) => e(v).size)

  }

  lazy val parser: Parser[Json.Arr] = Json.Parsers.arrOf(Json.Parsers.num)

  /** Parse from json string */
  def apply[R: Real](s: String)(implicit r: Real[R]): Vec[R] = seq2Vec(
    Json(s, parser).toArr.values.map {
      j: Json => r.json(j.toNum)
    })

  /** Parse sequence of doubles via [[Real.apply]] */
  def apply[R](doubles: Seq[Double])(implicit real: Real[R]): Vec[R] =
    doubles.map(real(_))
}

