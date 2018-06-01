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
/*
implicit class MatrixGroup[A](mat:Matrix) extends Ring[Matrix[A]] {


    lazy val zero = 
}
*/