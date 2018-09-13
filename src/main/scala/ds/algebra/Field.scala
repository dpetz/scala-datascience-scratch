package ds.algebra

trait Field[A] extends Ring[A] {
  def divide(l:A,r:A):A
  def inverse(v:A):A
}