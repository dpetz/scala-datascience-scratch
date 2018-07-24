package math.lina

/**
 * @see https://github.com/non/spire
 * @see https://twitter.github.io/algebird/
 */


trait Monoid[A] {
  def zero:A
  def plus(l:A, r:A):A
}

trait Group[A] extends Monoid[A] {
 def negate(v:A):A
 def minus(l:A, r:A):A
}

trait Ring[A] extends Group[A] {
    def one:A
    def times(l:A,r:A):A
}

trait Field[A] extends Ring[A] {
    def divide(l:A,r:A):A
    def inverse(v:A):A
}

class DoubleField extends Field[Double] {
  val zero = 0.0
  val one = 1.0

  def plus(l:Double,r:Double):Double = l+r
  def negate(v:Double):Double = -v
  def minus(l:Double, r:Double):Double = l-r
  def times(l:Double,r:Double):Double = l*r
  def divide(l:Double,r:Double):Double = l/r
  def inverse(v:Double):Double = 1/v

}

implicit class MatrixMath[A](matLeft:Matrix[A])(implicit f:Field[A]) {

    // context and view bounds
    //lazy val zero = Matrix.Rows(mat.elems(_ => field.zero) ))

    private def zip(matRight:Matrix[A]) = matLeft.elems(_.x).zip(matRight.elems(_.x))

    def +(matRight:Matrix[A]) = {
      assert( (matLeft.n == matRight.n) && (matLeft.m == matRight.m) )
      Matrix.Split(matLeft.m)(zip(matRight).map(case (l,r) => f.plus(l,r)))
    }
}
